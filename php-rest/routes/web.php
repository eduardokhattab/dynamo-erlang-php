<?php


use Illuminate\Http\Request;
use Aws\Kms\KmsClient;
use Aws\DynamoDb\DynamoDbClient;

/** @var \Laravel\Lumen\Routing\Router $router */

/*
|--------------------------------------------------------------------------
| Application Routes
|--------------------------------------------------------------------------
|
| Here is where you can register all of the routes for an application.
| It is a breeze. Simply tell Lumen the URIs it should respond to
| and give it the Closure to call when that URI is requested.
|
*/

$router->get('/', function () use ($router) {
    return $router->app->version();
});

$router->post('/save', function (Request $request) use ($router) {
    $this->validate($request, [
        'key' => 'required',
        'value' => 'required',
    ]);

    $key = $request->input('key');
    $value = $request->input('value');

    echo "Encrypting data with envelope encryption\n";

    $kmsClient = app('aws')->createKms();
    $dynamoDb = app('aws')->createDynamoDb();
    $tableName = env('DYNAMODB_TABLE');
    $kmsKeyId = env('KMS_KEY_ID');

    $dataKeyResult = $kmsClient->generateDataKey([
        'KeyId' => $kmsKeyId,
        'NumberOfBytes' => 32,
    ]);

    $plaintextKey = $dataKeyResult['Plaintext'];
    $encryptedKeyBlob = $dataKeyResult['CiphertextBlob'];

    echo sprintf(
        "Data Key generated - Plaintext: %d bytes, Encrypted: %d bytes\n",
        strlen($plaintextKey),
        strlen($encryptedKeyBlob)
    );

    $iv = random_bytes(12);
    $ciphertext = openssl_encrypt(
        $value,
        'aes-256-gcm',
        $plaintextKey,
        OPENSSL_RAW_DATA,
        $iv,
        $authTag
    );

    $payload = $iv . $authTag . $ciphertext;
    $encryptedValue = base64_encode($payload);

    echo sprintf(
        "Data encrypted (%d bytes -> %d bytes)\n",
        strlen($value),
        strlen($payload)
    );

    $base64EncryptedKey = base64_encode($encryptedKeyBlob);

    $dynamoDb->putItem([
        'TableName' => $tableName,
        'Item' => [
            'key' => ['S' => $key],
            'value' => ['S' => $encryptedValue],
            'data_key' => ['B' => $base64EncryptedKey]
        ]
    ]);

    echo "Encrypted Item saved on DynamoDB\n";

    return response()->json([
        'type' => 'set_response',
        'error' => 'ok',
        'key' => $key,
        'value' => $value
    ], 200);
});

$router->get('/get/{key}', function (Request $request, $key) use ($router) {
    try {
        echo "Retrieving and decrypting data for key: {$key}\n";

        $kmsClient = app('aws')->createKms();
        $dynamoDb = app('aws')->createDynamoDb();
        $tableName = env('DYNAMODB_TABLE');

        try {
            $result = $dynamoDb->getItem([
                'TableName' => $tableName,
                'Key' => [
                    'key' => ['S' => $key]
                ]
            ]);

            if (!isset($result['Item'])) {
                echo "Item not found\n";
                return response()->json([
                    'error' => 'not_found'
                ], 404);
            }

            $item = $result['Item'];
            $encryptedValue = $item['value']['S'];
            $rawDataKey = $item['data_key']['B'];

            echo "Item found in DynamoDB\n";
            echo sprintf("Encrypted value length: %d chars\n", strlen($encryptedValue));
            echo sprintf("Raw data key length: %d bytes\n", strlen($rawDataKey));

            $encryptedDataKey = base64_decode($rawDataKey);
            
            echo sprintf("After base64 decode - data key length: %d bytes\n", strlen($encryptedDataKey));

        } catch (Exception $e) {
            echo "Error getting from DynamoDB: " . $e->getMessage() . "\n";
            return response()->json([
                'error' => 'internal'
            ], 500);
        }

        try {
            echo "Decrypting data key with KMS\n";
            
            $decryptResult = $kmsClient->decrypt([
                'CiphertextBlob' => $encryptedDataKey
            ]);

            $plaintextKeyBase64 = base64_encode($decryptResult['Plaintext']);
            $plaintextKey = $decryptResult['Plaintext'];

            echo sprintf(
                "Data Key decrypted - Base64: %d bytes, Binary: %d bytes\n",
                strlen($plaintextKeyBase64),
                strlen($plaintextKey)
            );

        } catch (Exception $e) {
            echo "Error decrypting Data Key: " . $e->getMessage() . "\n";
            return response()->json([
                'error' => 'internal'
            ], 500);
        }

        try {
            echo "Decrypting data\n";
            
            $payload = base64_decode($encryptedValue);
            
            $iv = substr($payload, 0, 12);
            $authTag = substr($payload, 12, 16);
            $ciphertext = substr($payload, 28);

            echo sprintf("Payload breakdown - IV: 12 bytes, AuthTag: 16 bytes, CipherText: %d bytes\n", strlen($ciphertext));

            $originalValue = openssl_decrypt(
                $ciphertext,
                'aes-256-gcm',
                $plaintextKey,
                OPENSSL_RAW_DATA,
                $iv,
                $authTag
            );

            echo sprintf("Data decrypted (%d bytes)\n", strlen($originalValue));

            return response()->json([
                'key' => $key,
                'value' => $originalValue
            ], 200);

        } catch (Exception $e) {
            echo "Error decrypting data: " . $e->getMessage() . "\n";
            return response()->json([
                'error' => 'internal'
            ], 500);
        }

    } catch (Exception $e) {
        echo "Unexpected error: " . $e->getMessage() . "\n";
        return response()->json([
            'error' => 'internal'
        ], 500);
    }
});
