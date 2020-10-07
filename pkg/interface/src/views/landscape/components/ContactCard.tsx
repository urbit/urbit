import React from "react";
import { Sigil } from "~/logic/lib/sigil";
import * as Yup from "yup";

import { uxToHex } from "~/logic/lib/util";
import {
  ManagedForm as Form,
  Col,
  ManagedTextInputField as Input,
  Box,
  Text,
  Row,
} from "@tlon/indigo-react";
import { Formik, FormikHelpers } from "formik";
import { Contact } from "~/types/contact-update";
import { AsyncButton } from "~/views/components/AsyncButton";
import { ColorInput } from "~/views/components/ColorInput";
import GlobalApi from "~/logic/api/global";
import { ImageInput } from "~/views/components/ImageInput";
import { S3State } from "~/types";

interface ContactCardProps {
  contact: Contact;
  path: string;
  api: GlobalApi;
  s3: S3State;
}

const formSchema = Yup.object({
  color: Yup.string(),
  nickname: Yup.string(),
  email: Yup.string().matches(
    new RegExp(
      String(
        /[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*/.source
      ) +
        /@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?/
          .source
    ),
    "Not a valid email"
  ),
  phone: Yup.string().matches(
    new RegExp(
      String(/^\s*(?:\+?(\d{1,3}))?/.source) +
        /([-. (]*(\d{3})[-. )]*)?((\d{3})[-. ]*(\d{2,4})(?:[-.x ]*(\d+))?)\s*$/
          .source
    ),
    "Not a valid phone"
  ),

  website: Yup.string().matches(
    new RegExp(
      String(/[(http(s)?):\/\/(www\.)?a-zA-Z0-9@:%._\+~#=]{2,256}/.source) +
        /\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\+.~#?&//=]*)/.source
    ),
    "Not a valid website"
  ),
});

const emptyContact = {
  avatar: null,
  color: '0',
  nickname: '',
  email: '',
  phone: '',
  website: '', 
  notes: ''
};

export function ContactCard(props: ContactCardProps) {
  const us = `~${window.ship}`;
  const { contact } = props;
  const onSubmit = async (values: Contact, actions: FormikHelpers<Contact>) => {
    try {
      if(!contact) {
        const [,,ship] = props.path.split('/');
        values.color = uxToHex(values.color);
        await props.api.contacts.share(ship, props.path, us, values)
        actions.setStatus({ success: null });
        return;
      }

      await Object.keys(values).reduce((acc, key) => {
        const newValue = key !== "color" ? values[key] : uxToHex(values[key]);
        if (newValue !== contact[key]) {
          if (key === "avatar") {
            return acc.then(() =>
              props.api.contacts.edit(props.path, us, {
                avatar: { url: newValue },
              } as any)
            );
          }

          return acc.then(() =>
            props.api.contacts.edit(props.path, us, {
              [key]: newValue,
            } as any)
          );
        }
        return acc;
      }, Promise.resolve());
      actions.setStatus({ success: null });
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: e.message });
    }
  };

  const hexColor = contact?.color ? `#${uxToHex(contact.color)}` : "#000000";

  return (
    <Box p={4} height="100%" overflowY="auto">
      <Formik
        validationSchema={formSchema}
        initialValues={contact || emptyContact}
        onSubmit={onSubmit}
      >
        <Form
          display="grid"
          gridAutoRows="auto"
          gridTemplateColumns="100%"
          gridRowGap="5"
          maxWidth="400px"
        >
          <Row
            borderBottom={1}
            borderBottomColor="washedGray"
            pb={3}
            alignItems="center"
          >
            <Sigil size={32} classes="" color={hexColor} ship={us} />
            <Box ml={2}>
              <Text fontFamily="mono">{us}</Text>
            </Box>
          </Row>
          <ImageInput id="avatar" label="Avatar" s3={props.s3} />
          <ColorInput id="color" label="Sigil Color" />
          <Input id="nickname" label="Nickname" />
          <Input id="email" label="Email" />
          <Input id="phone" label="Phone" />
          <Input id="website" label="Website" />
          <Input id="notes" label="Notes" />
          <AsyncButton primary loadingText="Updating..." border>
            Save
          </AsyncButton>
        </Form>
      </Formik>
    </Box>
  );
}
