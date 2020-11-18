import React, { useEffect } from "react";
import { Box, Col, Text } from "@tlon/indigo-react";
import * as Yup from "yup";
import GlobalApi from "~/logic/api/global";

import { Groups, Associations, Association } from "~/types";
import { Formik, FormikHelpers, Form } from "formik";
import GroupSearch from "~/views/components/GroupSearch";
import { AsyncButton } from "~/views/components/AsyncButton";
import {useHistory} from "react-router-dom";

const formSchema = Yup.object({
  group: Yup.string().nullable(),
});

interface FormSchema {
  group: string | null;
}

interface GroupifyFormProps {
  groups: Groups;
  api: GlobalApi;
  associations: Associations;
  association: Association;
}

export function GroupifyForm(props: GroupifyFormProps) {
  const history = useHistory();
  const { association } = props;
  const onGroupify = async (
    values: FormSchema,
    actions: FormikHelpers<FormSchema>
  ) => {
    try {
      if (association["app-name"] === "chat") {
        await props.api.chat.groupify(
          association["app-path"],
          values.group,
          true
        );
      } else {
        const [, , ship, name] = association["app-path"].split("/");
        await props.api.graph.groupifyGraph(
          ship,
          name,
          values.group || undefined
        );
      }
      const mod = association.metadata.module || association['app-name'];
      const newGroup = values.group || association['group-path'];
      history.push(`/~landscape${newGroup}/resource/${mod}${association['app-path']}`);
      actions.setStatus({ success: null });
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: e.message });
    }
  };

  const groupPath = props.association?.["group-path"];

  const isUnmanaged = props.groups?.[groupPath]?.hidden || false;

  if (!isUnmanaged) {
    return null;
  }

  const initialValues: FormSchema = {
    group: null,
  };

  return (
    <Formik
      validationSchema={formSchema}
      initialValues={initialValues}
      onSubmit={onGroupify}
    >
      <Form>
        <Col flexShrink="0" gapY="4" maxWidth="512px">
          <Box>
            <Text fontWeight="500">Groupify this channel</Text>
          </Box>
          <GroupSearch
            id="group"
            label="Group"
            caption="Optionally, if you have admin privileges, you can add this channel to a group, or leave this blank to place the channel in its own group"
            groups={props.groups}
            associations={props.associations}
            adminOnly
          />
          <AsyncButton primary loadingText="Groupifying..." border>
            Groupify
          </AsyncButton>
        </Col>
      </Form>
    </Formik>
  );
}

export default GroupifyForm;
