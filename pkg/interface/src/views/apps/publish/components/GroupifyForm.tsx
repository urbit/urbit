import React, { useEffect } from "react";
import { Box, Col, Button, InputLabel, InputCaption } from "@tlon/indigo-react";
import * as Yup from "yup";
import GlobalApi from "~/logic/api/global";
import { Notebook } from "~/types/publish-update";
import { Contacts } from "~/types/contact-update";

import { MetadataForm } from "./MetadataForm";
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
  host: string;
  book: string;
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
      await props.api.graph.groupifyGraph(
        `~${window.ship}`, props.book, 'publish', values.group || undefined);
      actions.setStatus({ success: null });
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: e.message });
    }
  };

  const groupPath = props.association?.['group-path'];

  const isUnmanaged = props.groups?.[groupPath]?.hidden || false;

  if (!isUnmanaged) {
    return null;
  }

  const initialValues: FormSchema = {
    group: null
  };

  return (
    <Formik
      validationSchema={formSchema}
      initialValues={initialValues}
      onSubmit={onGroupify}
    >
      <Form style={{ display: "contents" }}>
        <GroupSearch
          id="group"
          label="Group"
          caption="What group should this notebook be added to? If blank, a new group will be made for the notebook"
          groups={props.groups}
          associations={props.associations}
          adminOnly
        />
        <AsyncButton loadingText="Groupifying..." border>
          Groupify
        </AsyncButton>
      </Form>
    </Formik>
  );
}

export default GroupifyForm;
