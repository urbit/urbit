import React, { useCallback } from "react";

import {
  ManagedTextInputField as Input,
  ManagedForm as Form,
  Box,
  Button,
  Col,
  Text,
  Menu,
} from "@tlon/indigo-react";

import { Formik } from "formik";
import GlobalApi from "../../../../api/global";
import { BucketList } from "./BucketList";
import { S3State } from "../../../../types";

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
    [api, s3]
  );
  return (
    <>
      <Col>
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
          <Form
            display="grid"
            gridTemplateColumns="100%"
            gridAutoRows="auto"
            gridRowGap={5}
          >
            <Box color="black" fontSize={1} fontWeight={900}>
              S3 Credentials
            </Box>
            <Input label="Endpoint" id="s3endpoint" />
            <Input label="Access Key ID" id="s3accessKeyId" />
            <Input
              type="password"
              label="Secret Access Key"
              id="s3secretAccessKey"
            />
            <Button style={{ cursor: 'pointer' }} type="submit">Submit</Button>
          </Form>
        </Formik>
      </Col>
      <Col>
        <Box color="black" mb={4} fontSize={1} fontWeight={700}>
          S3 Buckets
        </Box>
        <BucketList
          buckets={s3.configuration.buckets}
          selected={s3.configuration.currentBucket}
          api={api}
        />
      </Col>
    </>
  );
}
