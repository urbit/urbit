import React, { useState, useCallback, useEffect } from "react";
import { Body } from "~/views/components/Body";
import {
  Col,
  Row,
  Icon,
  Box,
  Text,
  ManagedTextInputField as Input,
} from "@tlon/indigo-react";
import { Formik, Form, FormikHelpers, useFormikContext } from "formik";
import { AsyncButton } from "~/views/components/AsyncButton";
import * as Yup from "yup";
import { Groups, Rolodex } from "~/types";
import { useWaitForProps } from "~/logic/lib/useWaitForProps";
import GlobalApi from "~/logic/api/global";
import { RouteComponentProps, useHistory } from "react-router-dom";
import urbitOb from "urbit-ob";
import { resourceFromPath } from "~/logic/lib/group";
import { StatelessAsyncButton } from "~/views/components/StatelessAsyncButton";
import { uxToHex, getModuleIcon } from "~/logic/lib/util";
import { FormError } from "~/views/components/FormError";
import { MetadataIcon } from "./MetadataIcon";
import { GroupSummary } from "./GroupSummary";

const formSchema = Yup.object({
  group: Yup.string()
    .required("Must provide group to join")
    .test("is-valid", "Invalid group", (group: string | null | undefined) => {
      if (!group) {
        return false;
      }
      const [patp, name] = group.split("/");
      return urbitOb.isValidPatp(patp) && name.length > 0;
    }),
});

interface FormSchema {
  group: string;
}

interface JoinGroupProps {
  groups: Groups;
  contacts: Rolodex;
  api: GlobalApi;
  autojoin?: string;
  inviteUid?: string;
}

function Autojoin(props: { autojoin: string | null }) {
  const { submitForm } = useFormikContext();

  useEffect(() => {
    if (props.autojoin) {
      submitForm();
    }
  }, []);

  return null;
}

export function JoinGroup(props: JoinGroupProps) {
  const { api, autojoin } = props;
  const history = useHistory();
  const initialValues: FormSchema = {
    group: autojoin || "",
  };

  const waiter = useWaitForProps(props);
  const [preview, setPreview] = useState<MetadataUpdatePreview | null>(null);

  const onConfirm = useCallback(async () => {
    const { group } = preview;
    await api.contacts.join(resourceFromPath(group));
    if (props.inviteUid) {
      api.invite.accept("contacts", props.inviteUid);
    }
    await waiter(({ contacts, groups }) => {
      return group in contacts && group in groups;
    });
    history.push(`/~landscape${group}`);
  }, [api, preview, waiter]);

  const onSubmit = useCallback(
    async (values: FormSchema, actions: FormikHelpers<FormSchema>) => {
      try {
        const [ship, name] = values.group.split("/");
        const path = `/ship/${ship}/${name}`;

        const prev = await api.metadata.preview(path);
        actions.setStatus({ success: null });
        setPreview(prev);
      } catch (e) {
        console.log(e);
        if (!(e instanceof Error)) {
          actions.setStatus({ error: "Unknown error" });
        } else if (e.message === "no-permissions") {
          actions.setStatus({
            error:
              "Unable to join group, you do not have the correct permissions",
          });
        } else if (e.message === "offline") {
          actions.setStatus({
            error: "Group host is offline, please try again later",
          });
        }
      }
    },
    [api, waiter, history]
  );

  return (
    <>
      <Col width="100%" alignItems="center" overflowY="auto" p="4">
        <Box mb={3}>
          <Text fontSize="2" fontWeight="bold">
            Join a Group
          </Text>
        </Box>
        {preview ? (
          <GroupSummary
            metadata={preview.metadata}
            memberCount={preview?.members}
            channelCount={preview?.["channel-count"]}
          >
            <Col
              gapY="2"
              p="2"
              borderRadius="2"
              border="1"
              borderColor="washedGray"
              bg="washedBlue"
            >
              <Text gray fontSize="1">
                Channels
              </Text>
              {Object.values(preview.channels).map(({ metadata }: any) => (
                <Row>
                  <Icon
                    mr="2"
                    color="blue"
                    icon={getModuleIcon(metadata.module) as any}
                  />
                  <Text color="blue">{metadata.title} </Text>
                </Row>
              ))}
            </Col>
            <StatelessAsyncButton primary name="join" onClick={onConfirm}>
              Join {preview.metadata.title}
            </StatelessAsyncButton>
          </GroupSummary>
        ) : (
          <Col width="100%" maxWidth="300px" gapY="4">
            <Formik
              validationSchema={formSchema}
              initialValues={initialValues}
              onSubmit={onSubmit}
            >
              <Form style={{ display: "contents" }}>
                <Autojoin autojoin={autojoin} />
                <Input
                  id="group"
                  label="Group"
                  caption="What group are you joining?"
                  placeholder="~sampel-palnet/test-group"
                />
                <AsyncButton mt="4">Join Group</AsyncButton>
                <FormError mt="4" />
              </Form>
            </Formik>
          </Col>
        )}
      </Col>
    </>
  );
}
