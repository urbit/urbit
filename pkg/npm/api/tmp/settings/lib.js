export const action = (data) => ({
    app: 'settings-store',
    mark: 'settings-event',
    json: data
});
export const putBucket = (desk, key, bucket) => action({
    'put-bucket': {
        desk,
        'bucket-key': key,
        'bucket': bucket
    }
});
export const delBucket = (desk, key) => action({
    'del-bucket': {
        desk,
        'bucket-key': key
    }
});
export const putEntry = (desk, bucket, key, value) => action({
    'put-entry': {
        desk,
        'bucket-key': bucket,
        'entry-key': key,
        value: value
    }
});
export const delEntry = (desk, bucket, key) => action({
    'del-entry': {
        desk,
        'bucket-key': bucket,
        'entry-key': key
    }
});
export const getAll = {
    app: 'settings-store',
    path: '/all'
};
export const getBucket = (desk, bucket) => ({
    app: 'settings-store',
    path: `/bucket/${bucket}`
});
export const getEntry = (desk, bucket, entry) => ({
    app: 'settings-store',
    path: `/entry/${desk}/${bucket}/${entry}`
});
export const getDeskSettings = (desk) => ({
    app: 'settings-store',
    path: `/desk/${desk}`
});
export * from './types';
//# sourceMappingURL=lib.js.map