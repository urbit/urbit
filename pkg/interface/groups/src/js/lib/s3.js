const AWS = require('aws-sdk')

export function configureClient() {
  // Configure client for use with Spaces
  const spacesEndpoint = new AWS.Endpoint('nyc3.digitaloceanspaces.com');
  return new AWS.S3({
      endpoint: spacesEndpoint,
      credentials: new AWS.Credentials({
        accessKeyId: 'ACCESS_KEY',
        secretAccessKey: 'SECRET_KEY'
      })
  });
}

export function createBucket() {
  // Create a new Space
  var params = {
      Bucket: "my-new-space-with-a-unique-name"
  };

  s3.createBucket(params, function(err, data) {
      if (err) console.log(err, err.stack);
      else     console.log(data);
  });
}

export function listBuckets() {
  // List all Spaces in the region
  s3.listBuckets({}, function(err, data) {
  if (err) console.log(err, err.stack);
      else {
          data['Buckets'].forEach(function(space) {
          console.log(space['Name']);
      })};
  });
}

export function putObject(body, bucket, key) {
  // Add a file to a Space
  var params = {
      Body: "The contents of the file",
      Bucket: "my-new-space-with-a-unique-name",
      Key: "file.ext",
  };

  s3.putObject(params, function(err, data) {
      if (err) console.log(err, err.stack);
      else     console.log(data);
  });
}

