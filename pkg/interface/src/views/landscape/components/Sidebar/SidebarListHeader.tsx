import React, { useCallback } from "react";
import * as Yup from "yup";
import {
  Row,
  Box,
  Icon,
  ManagedRadioButtonField as Radio,
  ManagedCheckboxField as Checkbox,
  Col,
  Text,
} from "@tlon/indigo-react";
import { FormikOnBlur } from "~/views/components/FormikOnBlur";
import { Dropdown } from "~/views/components/Dropdown";
import { FormikHelpers } from "formik";
import { SidebarListConfig, Workspace } from "./types";
import { Link, useHistory } from 'react-router-dom';
import {ShipSearch} from "~/views/components/ShipSearch";
import {Groups, Rolodex} from "~/types";

export function SidebarListHeader(props: {
  initialValues: SidebarListConfig;
  groups: Groups;
  contacts: Rolodex;
  baseUrl: string;
  selected: string;
  workspace: Workspace;
  handleSubmit: (c: SidebarListConfig) => void;
}) {

  const history = useHistory();
  const onSubmit = useCallback(
    (values: SidebarListConfig, actions: FormikHelpers<SidebarListConfig>) => {
      props.handleSubmit(values);
      actions.setSubmitting(false);
    },
    [props.handleSubmit]
  );

  return (
    <Row
      flexShrink="0"
      alignItems="center"
      justifyContent="space-between"
      py={2}
      pr={2}
      pl={2}
    >
      <Box flexShrink='0'>
        <Text bold>
          {props.initialValues.hideUnjoined ? "Joined Channels" : "All Channels"}
        </Text>
      </Box>
      <Box
        width='100%'
        textAlign='right'
        mr='2'
        display={(props.workspace?.type === 'home') ? 'inline-block' : 'none'}
      >
       <Link to={`${props.baseUrl}/invites`}>
          <Text
            display='inline-block'
            verticalAlign='middle'
            py='1px'
            px='3px'
            backgroundColor='washedBlue'
            color='blue'
            borderRadius='1'>
              + DM
            </Text>
        </Link>
      </Box>
      <Dropdown
        flexShrink='0'
        width="200px"
        alignY="top"
        alignX={["right", "left"]}
        options={
          <FormikOnBlur initialValues={props.initialValues} onSubmit={onSubmit}>
            <Col bg="white" borderRadius={1} border={1} borderColor="lightGray">
              <Col
                gapY="2"
                borderBottom={1}
                borderBottomColor="washedGray"
                p={2}
              >
                <Box>
                  <Text color="gray">Sort Order</Text>
                </Box>
                <Radio mb="1" label="A -> Z" id="asc" name="sortBy" />
                <Radio label="Last Updated" id="lastUpdated" name="sortBy" />
              </Col>
              <Col px={2}>
                <Checkbox
                  my={3}
                  id="hideUnjoined"
                  label="Hide Unsubscribed Channels"
                />
              </Col>
            </Col>
          </FormikOnBlur>
        }
      >
        <Icon color="gray" icon="Adjust" />
      </Dropdown>
    </Row>
  );
}
