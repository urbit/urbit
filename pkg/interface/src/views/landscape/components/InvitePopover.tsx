import React, { useCallback, useRef, useMemo } from "react";
import { Box, Text, Col, Button, Row } from "@tlon/indigo-react";
import * as Yup from 'yup';

import { ShipSearch } from "~/views/components/ShipSearch";
import { Association } from "~/types/metadata-update";
import { Switch, Route, useHistory } from "react-router-dom";
import { Formik, Form } from "formik";
import { AsyncButton } from "~/views/components/AsyncButton";
import { useOutsideClick } from "~/logic/lib/useOutsideClick";
import { FormError } from "~/views/components/FormError";
import { resourceFromPath } from "~/logic/lib/group";
import GlobalApi from "~/logic/api/global";
import { Groups, Rolodex } from "~/types";
import { ChipInput } from "~/views/components/ChipInput";

interface InvitePopoverProps {
  baseUrl: string;
  association: Association;
  groups: Groups;
  contacts: Rolodex;
  api: GlobalApi;
}

interface FormSchema {
  emails: string[];
  ships: string[];
}

const formSchema = Yup.object({
  emails: Yup.array(Yup.string().email("Invalid email")),
  ships: Yup.array(Yup.string())
});

export function InvitePopover(props: InvitePopoverProps) {
  const { baseUrl, api, association } = props;

  const relativePath = (p: string) => baseUrl + p;
  const { title } = association?.metadata || "";
  const innerRef = useRef(null);
  const history = useHistory();

  const onOutsideClick = useCallback(() => {
    history.push(props.baseUrl);
  }, [history.push, props.baseUrl]);
  useOutsideClick(innerRef, onOutsideClick);

  const onSubmit = async ({ ships, emails }: { ships: string[] }, actions) => {
    //  TODO: how to invite via email?
    try {
      const resource = resourceFromPath(association["group-path"]);
      await ships.reduce(
        (acc, s) => acc.then(() => api.contacts.invite(resource, `~${s}`)),
        Promise.resolve()
      );
      actions.setStatus({ success: null });
      onOutsideClick();
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: e.message });
    }
  };

  const initialValues: FormSchema = { ships: [], emails: [] };


  return (
    <Switch>
      <Route path={[relativePath("/invites")]}>
        <Box
          display="flex"
          justifyContent="center"
          alignItems="center"
          bg="gray"
          left="0px"
          top="0px"
          width="100vw"
          height="100vh"
          zIndex={4}
          position="fixed"
        >
          <Box
            ref={innerRef}
            border={1}
            borderColor="washedGray"
            borderRadius={1}
            maxHeight="472px"
            width="380px"
            bg="white"
          >
            <Formik
              initialValues={initialValues}
              onSubmit={onSubmit}
              validationSchema={formSchema}
            >
              <Form>
                <Col gapY="3" p={3}>
                  <Box>
                    <Text>Invite to </Text>
                    <Text fontWeight="800">{title}</Text>
                  </Box>
                  <ShipSearch
                    groups={props.groups}
                    contacts={props.contacts}
                    id="ships"
                    label=""
                  />
                  <FormError message="Failed to invite" />
                  <ChipInput
                    id="emails"
                    label="Invite via Email"
                    caption="Send an Urbit ID and invite them to this group"
                    placeholder="name@example.com"
                    breakOnSpace
                  />
                </Col>
                <Row
                  borderTop={1}
                  borderTopColor="washedGray"
                  justifyContent="flex-end"
                >
                  <AsyncButton
                    border={0}
                    color="blue"
                    loadingText="Inviting..."
                  >
                    Send
                  </AsyncButton>
                </Row>
              </Form>
            </Formik>
          </Box>
        </Box>
      </Route>
    </Switch>
  );
}
