<?php


use Illuminate\Http\Request;
use Aws\Kms\KmsClient;
use Aws\DynamoDb\DynamoDbClient;
use OpenApi\Annotations as OA;

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

/**
 * @OA\Info(
 *     title="HTTP API for DynamoDB and KMS",
 *     version="1.0.0",
 *     description="API for saving and retrieving encrypted data using AWS KMS and DynamoDB with envelope encryption",
 * )
 * 
 * @OA\Server(
 *     url="/",
 *     description="API Server"
 * )
 * 
 * @OA\Tag(
 *     name="Key/Value",
 *     description="Data encryption and decryption operations"
 * )
 */

$router->get('/', function () use ($router) {
    return $router->app->version();
});

/**
 * @OA\Post(
 *     path="/api/save",
 *     operationId="saveEncryptedData",
 *     tags={"Key/Value"},
 *     summary="Save encrypted data",
 *     description="Encrypts and saves data",
 *     @OA\RequestBody(
 *         required=true,
 *         description="Data to be encrypted and saved",
 *         @OA\MediaType(
 *             mediaType="application/json",
 *             @OA\Schema(
 *                 required={"key", "value"},
 *                 @OA\Property(
 *                     property="key",
 *                     type="string",
 *                     description="Unique key to identify the data",
 *                     example="key"
 *                 ),
 *                 @OA\Property(
 *                     property="value",
 *                     type="string",
 *                     description="Value to be encrypted and stored",
 *                     example="value"
 *                 )
 *             )
 *         )
 *     ),
 *     @OA\Response(
 *         response=201,
 *         description="Data saved successfully",
 *         @OA\MediaType(
 *             mediaType="application/json",
 *             @OA\Schema(
 *                 @OA\Property(
 *                     property="type",
 *                     type="string",
 *                     example="set_response"
 *                 ),
 *                 @OA\Property(
 *                     property="error",
 *                     type="string",
 *                     example="ok"
 *                 ),
 *                 @OA\Property(
 *                     property="key",
 *                     type="string",
 *                     example="key"
 *                 ),
 *                 @OA\Property(
 *                     property="value",
 *                     type="string",
 *                     description="Original value (decrypted) returned for confirmation",
 *                     example="value"
 *                 )
 *             )
 *         )
 *     ),
 *     @OA\Response(
 *         response=422,
 *         description="Validation error",
 *         @OA\MediaType(
 *             mediaType="application/json",
 *             @OA\Schema(
 *                 @OA\Property(
 *                     property="value",
 *                     type="array",
 *                     @OA\Items(type="string", example="The value field is required.")
 *                 )
 *             )
 *         )
 *     ),
 *     @OA\Response(
 *         response=500,
 *         description="Internal server error",
 *         @OA\MediaType(
 *             mediaType="application/json",
 *             @OA\Schema(
 *                 @OA\Property(
 *                     property="message",
 *                     type="string",
 *                     example="Server Error"
 *                 )
 *             )
 *         )
 *     )
 * )
 */
$router->post('/api/save', function (Request $request) use ($router) {
    $this->validate($request, [
        'key' => 'required',
        'value' => 'required',
    ]);

    $key = $request->input('key');
    $value = $request->input('value');

    Log::info("Encrypting data with envelope encryption");

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

    Log::debug('Data key generated successfully', [
        'Plaintext' => strlen($plaintextKey),
        'Encrypted' => strlen($encryptedKeyBlob)
    ]);

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

    Log::debug('Data encrypted', [
        'original_size_bytes' => strlen($value),
        'encrypted_payload_size_bytes' => strlen($payload)
    ]);

    $base64EncryptedKey = base64_encode($encryptedKeyBlob);

    $dynamoDb->putItem([
        'TableName' => $tableName,
        'Item' => [
            'key' => ['S' => $key],
            'value' => ['S' => $encryptedValue],
            'data_key' => ['B' => $base64EncryptedKey]
        ]
    ]);

    Log::info('Encrypted item saved to DynamoDB successfully\n');

    return response()->json([
        'key' => $key,
        'value' => $value
    ], 201);
});

/**
 * @OA\Get(
 *     path="/api/get/{key}",
 *     operationId="getDecryptedData",
 *     tags={"Key/Value"},
 *     summary="Retrieve decrypted data",
 *     description="Retrieves and decrypts data using the provided key",
 *     @OA\Parameter(
 *         name="key",
 *         in="path",
 *         required=true,
 *         description="Unique key to identify the stored data",
 *         @OA\Schema(
 *             type="string",
 *             example="key"
 *         )
 *     ),
 *     @OA\Response(
 *         response=200,
 *         description="Data retrieved and decrypted successfully",
 *         @OA\MediaType(
 *             mediaType="application/json",
 *             @OA\Schema(
 *                 @OA\Property(
 *                     property="key",
 *                     type="string",
 *                     description="Key used to retrieve the data",
 *                     example="key"
 *                 ),
 *                 @OA\Property(
 *                     property="value",
 *                     type="string",
 *                     description="Decrypted value",
 *                     example="value"
 *                 )
 *             )
 *         )
 *     ),
 *     @OA\Response(
 *         response=404,
 *         description="Key not found",
 *         @OA\MediaType(
 *             mediaType="application/json",
 *             @OA\Schema(
 *                 @OA\Property(
 *                     property="error",
 *                     type="string",
 *                     example="not_found"
 *                 )
 *             )
 *         )
 *     ),
 *     @OA\Response(
 *         response=500,
 *         description="Internal server error",
 *         @OA\MediaType(
 *             mediaType="application/json",
 *             @OA\Schema(
 *                 @OA\Property(
 *                     property="error",
 *                     type="string",
 *                     example="internal"
 *                 )
 *             )
 *         )
 *     )
 * )
 */
$router->get('/api/get/{key}', function (Request $request, $key) use ($router) {
    try {
        Log::info("Retrieving and decrypting data for key: {$key}");

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
                Log::warning('Item not found in DynamoDB', [
                    'key' => $key,
                    'table' => $tableName
                ]);
                return response()->json([
                    'error' => 'not_found'
                ], 404);
            }

            $item = $result['Item'];
            $encryptedValue = $item['value']['S'];
            $rawDataKey = $item['data_key']['B'];

            Log::info('Item found in DynamoDB', [
                'key' => $key,
                'encrypted_value_length' => strlen($encryptedValue),
                'raw_data_key_length' => strlen($rawDataKey)
            ]);

            $encryptedDataKey = base64_decode($rawDataKey);
            
            Log::debug('Data key decoded from base64', [
                'decoded_key_length' => strlen($encryptedDataKey)
            ]);

        } catch (Exception $e) {
            Log::error('Error retrieving item from DynamoDB', [
                'error' => $e->getMessage(),
                'key' => $key,
                'table' => $tableName,
                'trace' => $e->getTraceAsString()
            ]);
            return response()->json([
                'error' => 'internal'
            ], 500);
        }

        try {
            Log::info("Decrypting data key with KMS");
            
            $decryptResult = $kmsClient->decrypt([
                'CiphertextBlob' => $encryptedDataKey
            ]);

            $plaintextKeyBase64 = base64_encode($decryptResult['Plaintext']);
            $plaintextKey = $decryptResult['Plaintext'];

            Log::debug('Data key decrypted successfully', [
                'base64_key_length' => strlen($plaintextKeyBase64),
                'binary_key_length' => strlen($plaintextKey)
            ]);

        } catch (Exception $e) {
            Log::error('Error decrypting data key with KMS', [
                'error' => $e->getMessage(),
                'key' => $key,
                'trace' => $e->getTraceAsString()
            ]);
            return response()->json([
                'error' => 'internal'
            ], 500);
        }

        try {
            Log::info("Decrypting data");
            
            $payload = base64_decode($encryptedValue);
            
            $iv = substr($payload, 0, 12);
            $authTag = substr($payload, 12, 16);
            $ciphertext = substr($payload, 28);

            Log::debug('Payload breakdown completed', [
                'iv_length' => strlen($iv),
                'auth_tag_length' => strlen($authTag),
                'ciphertext_length' => strlen($ciphertext)
            ]);

            $originalValue = openssl_decrypt(
                $ciphertext,
                'aes-256-gcm',
                $plaintextKey,
                OPENSSL_RAW_DATA,
                $iv,
                $authTag
            );

            Log::debug('Data decrypted successfull\n', [
                'key' => $key,
                'decrypted_data_length' => strlen($originalValue)
            ]);

            return response()->json([
                'key' => $key,
                'value' => $originalValue
            ], 200);

        } catch (Exception $e) {
            Log::error('Error during data decryption', [
                'error' => $e->getMessage(),
                'key' => $key,
                'trace' => $e->getTraceAsString()
            ]);
            return response()->json([
                'error' => 'internal'
            ], 500);
        }

    } catch (Exception $e) {
        Log::error('Unexpected error during retrieval process', [
            'error' => $e->getMessage(),
            'key' => $key,
            'trace' => $e->getTraceAsString()
        ]);
        return response()->json([
            'error' => 'internal'
        ], 500);
    }
});
