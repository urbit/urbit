import React, { useEffect } from "react";
import { Box, Col, Text } from "@tlon/indigo-react";
import * as Yup from "yup";
import GlobalApi from "~/logic/api/global";

import { Groups, Associations, Association } from "~/types";
import { Formik, FormikHelpers, Form } from "formik";
import GroupSearch from "~/views/components/GroupSearch";
import { AsyncButton } from "~/views/components/AsyncButton";

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
  const onGroupify = async (
    values: FormSchema,
    actions: FormikHelpers<FormSchema>
  ) => {
    try {
      const [, , ship, name] = props.association["app-path"].split("/");
      await props.api.graph.groupifyGraph(
        ship,
        name,
        values.group || undefined
      );
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
        <Col gapY="4" maxWidth="512px">
          <Box>
            <Text fontWeight="500">Groupify this chanel</Text>
          </Box>
          <GroupSearch
            id="group"
            label="Group"
            caption="What group should this channel be added to? If blank, a new group will be made for the channel"
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
