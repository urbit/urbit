import React, { ReactElement, useCallback, useState } from "react";
import { Formik, FormikHelpers } from 'formik';

import {
  ManagedTextInputField as Input,
  ManagedForm as Form,
  Box,
  Button,
  Text,
  Menu,
  MenuButton,
  MenuList,
  MenuItem,
  Row,
} from "@tlon/indigo-react";

import GlobalApi from '~/logic/api/global';

export function BucketList({
  buckets,
  selected,
  api
}: {
  buckets: Set<string>;
  selected: string;
  api: GlobalApi;
}): ReactElement {
  const _buckets = Array.from(buckets);

  const [adding, setAdding] = useState(false);

  const onSubmit = useCallback(
    (values: { newBucket: string }, actions: FormikHelpers<any>) => {
      api.s3.addBucket(values.newBucket);
      actions.resetForm({ values: { newBucket: "" } });
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
    <Formik initialValues={{ newBucket: '' }} onSubmit={onSubmit}>
      <Form
        display="grid"
        gridTemplateColumns="100%"
        gridAutoRows="auto"
        gridRowGap={2}
      >
        {_buckets.map(bucket => (
          <Box
            key={bucket}
            display="flex"
            justifyContent="space-between"
            alignItems="center"
            borderRadius={1}
            border={1}
            borderColor="lightGray"
            fontSize={1}
            pl={2}
            mb={2}
          >
            <Text>{bucket}</Text>
            {bucket === selected && (
              <Text p={2} color="green">
                Active
              </Text>
            )}
            {bucket !== selected && (
              <Menu>
                <MenuButton border={0} cursor="pointer" width="auto">
                  Options
                </MenuButton>
                <MenuList>
                  <MenuItem onSelect={onSelect(bucket)}>Make Active</MenuItem>
                  <MenuItem onSelect={onDelete(bucket)}>Delete</MenuItem>
                </MenuList>
              </Menu>
            )}
          </Box>
        ))}
        {adding && (
          <Input
            placeholder="Enter your new bucket"
            mt="2"
            label="New Bucket"
            id="newBucket"
          />
        )}
        <Row gapX="3" mt="3">
          <Button type="button" onClick={() => setAdding(false)}>
            Cancel
          </Button>
          <Button
            width="fit-content"
            primary
            type={adding ? "submit" : "button"}
            onClick={() => setAdding((s) => !s)}
          >
            {adding ? "Submit" : "Add new bucket"}
          </Button>
        </Row>
      </Form>
    </Formik>
  );
}
