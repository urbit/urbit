import React, { useCallback } from "react";
import * as Yup from "yup";
import { Row, Box, Icon, Radio, Col, Checkbox } from "@tlon/indigo-react";
import { FormikOnBlur } from "./FormikOnBlur";
import { Dropdown } from "./Dropdown";
import { FormikHelpers } from "formik";

export type SidebarSort = "asc" | "desc";

export interface SidebarListConfig {
  sortBy: SidebarSort;
  hideUnjoined: boolean;
}

export function SidebarListHeader(props: {
  initialValues: SidebarListConfig;
  handleSubmit: (c: SidebarListConfig) => void;
}) {
  const onSubmit = useCallback(
    (values: SidebarListConfig, actions: FormikHelpers<SidebarListConfig>) => {
      props.handleSubmit(values);
      actions.setSubmitting(false);
    },
    [props.handleSubmit]
  );

  return (
    <Row alignItems="center" justifyContent="space-between" py={2} px={3}>
      <Box>
        {props.initialValues.hideUnjoined ? "Joined Channels" : "All Channels"}
      </Box>
      <Dropdown
        width="200px"
        alignY="top"
        options={
          <FormikOnBlur initialValues={props.initialValues} onSubmit={onSubmit}>
            <Col>
              <Col borderBottom={1} borderBottomColor="washedGray" p={2}>
                <Box color="gray" mt={2} mb={4}>
                  Sort Order
                </Box>
                <Radio label="A -> Z" id="asc" name="sortBy" />
                <Radio label="Z -> A" id="desc" name="sortBy" />
              </Col>
              <Col px={2}>
                <Checkbox
                  mt={4}
                  id="hideUnjoined"
                  label="Hide Unsubscribed Channels"
                />
              </Col>
            </Col>
          </FormikOnBlur>
        }
      >
        <Icon stroke="gray" icon="Menu" />
      </Dropdown>
    </Row>
  );
}
