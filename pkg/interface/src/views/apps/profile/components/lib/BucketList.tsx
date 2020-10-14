import React, { useCallback } from "react";

import {
  Input,
  Box,
  Button,
  Col,
  Text,
  Menu,
  MenuButton,
  MenuList,
  MenuItem,
} from "@tlon/indigo-react";
import { Formik, Form } from "formik";

import GlobalApi from "../../../../api/global";

export function BucketList({
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
            mt={2}
            type="text"
            label="New Bucket"
            id="newBucket"
          />
          <Button border borderColor="washedGrey" type="submit">
            Add
          </Button>
        </Col>
      </Form>
    </Formik>
  );
}
