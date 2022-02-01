const s3Action = (data) => ({
    app: 's3-store',
    mark: 's3-action',
    json: data
});
export const setCurrentBucket = (bucket) => s3Action({
    'set-current-bucket': bucket
});
export const addBucket = (bucket) => s3Action({
    'add-bucket': bucket
});
export const removeBucket = (bucket) => s3Action({
    'remove-bucket': bucket
});
export const setEndpoint = (endpoint) => s3Action({
    'set-endpoint': endpoint
});
export const setAccessKeyId = (accessKeyId) => s3Action({
    'set-access-key-id': accessKeyId
});
export const setSecretAccessKey = (secretAccessKey) => s3Action({
    'set-secret-access-key': secretAccessKey
});
//# sourceMappingURL=lib.js.map