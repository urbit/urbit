import React, { useCallback } from "react";

import {
  Input,
  Box,
  Center,
  Button,
  Checkbox,
  Col,
  Text,
  Menu,
  MenuButton,
  MenuList,
  MenuItem,
} from "@tlon/indigo-react";

import { Formik, Form } from "formik";
import * as Yup from "yup";
import GlobalApi from "../../../../api/global";
import { S3State } from "../../../../types/s3-update";

function BucketList({
  buckets,
  selected,
  api,
}: {
  buckets: Set<string>;
  selected: string;
  api: GlobalApi;
}) {
  const _buckets = Array.from(buckets);

  const onSubmit = useCallback(
    (values: { newBucket: string }) => {
      api.s3.addBucket(values.newBucket);
    },
    [api]
  );

  const onSelect = useCallback(
    (bucket: string) => {
      return function () {
        api.s3.setCurrentBucket(bucket);
      };
    },
    [api]
  );

  const onDelete = useCallback(
    (bucket: string) => {
      return function () {
        api.s3.removeBucket(bucket);
      };
    },
    [api]
  );

  return (
    <Formik initialValues={{ newBucket: "" }} onSubmit={onSubmit}>
      <Form>
        <Col alignItems="start">
          {_buckets.map((bucket) => (
            <Box
              key={bucket}
              display="flex"
              justifyContent="space-between"
              alignItems="center"
              borderRadius={1}
              border={1}
              borderColor="washedGray"
              fontSize={1}
              pl={2}
              mb={2}
              width="100%"
            >
              <Text>{bucket}</Text>
              {bucket === selected && (
                <Text p={1} color="green">
                  Active
                </Text>
              )}
              {bucket !== selected && (
                <Menu>
                  <MenuButton sm>Options</MenuButton>
                  <MenuList>
                    <MenuItem onSelect={onSelect(bucket)}>Make Active</MenuItem>
                    <MenuItem onSelect={onDelete(bucket)}>Delete</MenuItem>
                  </MenuList>
                </Menu>
              )}
            </Box>
          ))}
          <Input
            mt={4}
            type="text"
            label="New Bucket"
            id="newBucket"
            name="newBucket"
          />
          <Button border={1} borderColor="washedGrey" type="submit">
            Add
          </Button>
        </Col>
      </Form>
    </Formik>
  );
}

interface FormSchema {
  s3bucket: string;
  s3buckets: string[];
  s3endpoint: string;
  s3accessKeyId: string;
  s3secretAccessKey: string;
}

interface S3FormProps {
  api: GlobalApi;
  s3: S3State;
}

export default function S3Form(props: S3FormProps) {
  const { api, s3 } = props;

  const onSubmit = useCallback(
    (values: FormSchema) => {
      if (values.s3secretAccessKey !== s3.credentials?.secretAccessKey) {
        api.s3.setSecretAccessKey(values.s3secretAccessKey);
      }

      if (values.s3endpoint !== s3.credentials?.endpoint) {
        api.s3.setEndpoint(values.s3endpoint);
      }

      if (values.s3accessKeyId !== s3.credentials?.accessKeyId) {
        api.s3.setAccessKeyId(values.s3accessKeyId);
      }
    },
    [api]
  );
  return (
    <Box
      display="grid"
      gridTemplateColumns="1fr"
      gridTemplateRows="auto"
      gridRowGap={4}
      justifyItems="start"
    >
      <Box color="black" fontSize={1} mb={4} mt={7} fontWeight={900}>
        S3 Credentials
      </Box>
      <Formik
        initialValues={
          {
            s3bucket: s3.configuration.currentBucket,
            s3buckets: Array.from(s3.configuration.buckets),
            s3endpoint: s3.credentials?.endpoint,
            s3accessKeyId: s3.credentials?.accessKeyId,
            s3secretAccessKey: s3.credentials?.secretAccessKey,
          } as FormSchema
        }
        onSubmit={onSubmit}
      >
        <Form>
          <Input width="256px" type="text" label="Endpoint" id="s3endpoint" />
          <Input
            width="256px"
            type="text"
            label="Access Key ID"
            id="s3accessKeyId"
          />
          <Input
            width="256px"
            type="password"
            label="Secret Access Key"
            id="s3secretAccessKey"
          />
          <Button border={1} type="submit">
            Submit
          </Button>
        </Form>
      </Formik>
      <Box color="black" fontSize={1} my={2} fontWeight={700}>
        S3 Buckets
      </Box>
      <BucketList
        buckets={s3.configuration.buckets}
        selected={s3.configuration.currentBucket}
        api={api}
      />
    </Box>
  );
}
