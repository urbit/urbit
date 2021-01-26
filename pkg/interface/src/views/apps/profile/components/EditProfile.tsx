import React from "react";
import * as Yup from "yup";

import {
  Center,
  Box,
  Text,
  Row,
  Button,
} from "@tlon/indigo-react";
import { Formik, FormikHelpers } from "formik";
import { useHistory } from "react-router-dom";

import GlobalApi from "~/logic/api/global";
import { Sigil } from "~/logic/lib/sigil";
import { AsyncButton } from "~/views/components/AsyncButton";
import { ColorInput } from "~/views/components/ColorInput";
import { ImageInput } from "~/views/components/ImageInput";


const formSchema = Yup.object({
  nickname: Yup.string(),
  bio: Yup.string(),
  color: Yup.string()
});

const emptyContact = {
  nickname: '',
  bio: '',
  status: '',
  color: '0',
  avatar: null,
  cover: null,
  groups: [],
  'last-updated': 0
};


export function EditProfile(props: any) {
  const { contact, ship } = props;
  if (ship !== window.ship) {
    return null;
  }
  const history = useHistory();

  const onSubmit = async (values: any, actions: FormikHelpers<Contact>) => {
    try {
      if(!contact) {
        const [,,ship] = props.path.split('/');
        values.color = uxToHex(values.color);
        const sharedValues = Object.assign({}, values);
        sharedValues.avatar = !!values.avatar ? values.avatar : null;
        console.log(values);
        await props.api.contacts.share(ship, props.path, us, sharedValues);
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



  return (
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
        width="100%"
      >
        <Row
          borderBottom={1}
          borderBottomColor="washedGray"
          pb={3}
          alignItems="center"
        >
          <Box height='32px' width='32px'>
            {image}
          </Box>
          <Box ml={2}>
            <Text mono={!Boolean(nickname)}>{nickname}</Text>
          </Box>
        </Row>
        <ImageInput id="avatar" label="Avatar" s3={props.s3} />
        <ColorInput id="color" label="Sigil Color" />
        <Input id="nickname" label="Nickname" />
        <Input id="bio" label="Email" />
        <Input id="phone" label="Phone" />
        <Input id="website" label="Website" />
        <Input id="notes" label="Notes" />
        <AsyncButton primary loadingText="Updating..." border>
          {(contact) ? "Save" : "Share Contact"}
        </AsyncButton>
      </Form>
    </Formik>
  );
}
