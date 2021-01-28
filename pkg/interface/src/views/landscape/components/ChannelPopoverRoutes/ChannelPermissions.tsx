import React from "react";
import _ from "lodash";
import * as Yup from "yup";
import {
  Label,
  ManagedTextInputField as Input,
  ManagedToggleSwitchField as Checkbox,
  Box,
  Col,
  Text,
  Row,
  ManagedRadioButtonField as Radio,
} from "@tlon/indigo-react";
import { Formik, Form, FormikHelpers } from "formik";
import { PermVariation, Association, Group, Groups, Rolodex } from "~/types";
import { ShipSearch } from "~/views/components/ShipSearch";
import GlobalApi from "~/logic/api/global";
import { resourceFromPath } from "~/logic/lib/group";
import { AsyncButton } from "~/views/components/AsyncButton";
import { FormSubmit } from "~/views/components/FormSubmit";

function PermissionsSummary(props: {
  writersSize: number;
  vip: PermVariation;
}) {
  const { writersSize, vip } = props;

  const description =
    writersSize === 0
      ? "Currently, all members of the group can write to this channel"
      : `Currently, only ${writersSize} ship${
          writersSize > 1 ? "s" : ""
        } can write to this channel`;

  const vipDescription =
    vip === "reader-comments" && writersSize !== 0
      ? ". All ships may comment"
      : "";

  return (
    <Box
      p="2"
      border="1"
      borderColor="lightBlue"
      borderRadius="1"
      backgroundColor="washedBlue"
    >
      <Text>
        {description}
        {vipDescription}
      </Text>
    </Box>
  );
}

interface GraphPermissionsProps {
  association: Association;
  group: Group;
  groups: Groups;
  contacts: Rolodex;
  api: GlobalApi;
}

interface FormSchema {
  writePerms: "self" | "everyone" | "subset";
  writers: string[];
  readerComments: boolean;
}

const formSchema = (members: string[]) => {
  return Yup.object({
    writePerms: Yup.string(),
    writers: Yup.array(Yup.string().oneOf(members, "${value} is not in group")),
    readerComments: Yup.boolean(),
  });
};

export function GraphPermissions(props: GraphPermissionsProps) {
  const { api, group, association } = props;

  const writers = _.get(
    group.tags,
    ["graph", association.resource, "writers"],
    new Set()
  );

  let [, , hostShip] = association.resource.split("/");
  hostShip = hostShip.slice(1);

  const writePerms =
    writers.size === 0
      ? ("everyone" as const)
      : writers.size === 1 && writers.has(hostShip)
      ? ("self" as const)
      : ("subset" as const);

  const readerComments = association.metadata.vip === "reader-comments";

  const initialValues = {
    writePerms,
    writers: Array.from(writers)
      .filter((x) => x !== hostShip)
      .map((s) => `~${s}`),
    readerComments: association.metadata.vip === "reader-comments",
  };

  const onSubmit = async (values: FormSchema, actions) => {
    const resource = resourceFromPath(association.group);
    const tag = {
      app: "graph",
      resource: association.resource,
      tag: "writers",
    };
    const allWriters = Array.from(writers).map(w => `~${w}`);
    if (values.readerComments !== readerComments) {
      await api.metadata.update(association, {
        vip: values.readerComments ? "reader-comments" : "",
      });
    }

    if (values.writePerms === "everyone") {
      if (writePerms === "everyone") {
        actions.setStatus({ success: null });
        return;
      }
      await api.groups.removeTag(
        resource,
        tag,
        allWriters
      );
    } else if (values.writePerms === "self") {
      if (writePerms === "self") {
        actions.setStatus({ success: null });
        return;
      }
      await api.groups.removeTag(
        resource,
        tag,
        allWriters
      );
      await api.groups.addTag(resource, tag, [`~${hostShip}`]);
      actions.setStatus({ success: null });
    } else if (values.writePerms === "subset") {
      const toRemove = _.difference(allWriters, values.writers)

      const toAdd = [..._.difference(values.writers, allWriters), `~${hostShip}`];

      toRemove.length > 0 && await api.groups.removeTag(resource, tag, toRemove);
      toAdd.length > 0 && await api.groups.addTag(resource, tag, toAdd);

      actions.setStatus({ success: null });
    }
  };

  const schema = formSchema(Array.from(group.members).map(m => `~${m}`));

  return (
    <Formik
      validationSchema={schema}
      initialValues={initialValues}
      onSubmit={onSubmit}
    >
      {({ values }) => (
        <Form style={{ display: "contents" }}>
          <Col mt="4" flexShrink={0} gapY="5">
            <Col gapY="1">
              <Text id="permissions" fontWeight="bold" fontSize="2">
                Permissions
              </Text>
              <Text gray>
                Add or remove read/write privileges to this channel. Group
                admins can always write to a channel
              </Text>
            </Col>
            <Col>
              <Label mb="2">Permissions Summary</Label>
              <PermissionsSummary
                writersSize={writers.size}
                vip={association.metadata.vip}
              />
            </Col>
            <Col gapY="3">
              <Label> Write Access</Label>
              <Radio
                name="writePerms"
                id="everyone"
                label="All group members"
              />
              <Radio name="writePerms" id="self" label="Only host" />
              <Radio
                name="writePerms"
                id="subset"
                label="Host and selected ships"
              />
              {values.writePerms === "subset" && (
                <ShipSearch
                  groups={props.groups}
                  contacts={props.contacts}
                  id="writers"
                  label=""
                  maxLength={undefined}
                />
              )}
            </Col>
            {association.metadata.module !== "chat" && (
              <Checkbox
                id="readerComments"
                label="Allow readers to comment"
                caption="If enabled, all members of the group can comment on this channel"
              />
            )}
            <FormSubmit>
              Update Permissions
            </FormSubmit>
          </Col>
        </Form>
      )}
    </Formik>
  );
}
