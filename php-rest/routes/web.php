<?php


use Illuminate\Http\Request;
use Aws\Kms\KmsClient;
use Aws\DynamoDb\DynamoDbClient;
// use Ramsey\Uuid\Uuid;

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
