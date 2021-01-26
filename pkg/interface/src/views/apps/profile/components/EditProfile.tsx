import React from "react";
import * as Yup from "yup";

import {
  ManagedForm as Form,
  ManagedTextInputField as Input,
  Center,
  Col,
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
  color: Yup.string(),
  avatar: Yup.string().nullable()
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
          return acc.then(() =>
            props.api.contacts.edit(ship, { [key]: newValue })
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
      <Form width="100%" p={2}>
        <Input id="nickname" label="Name" mb={3} />
        <Input id="bio" label="Description" mb={3} />
        <Row mb={3} width="100%">
          <Col pr={2} width="40%">
            <ColorInput id="color" label="Sigil Color" />
          </Col>
          <Col pl={2} width="60%">
            <ImageInput id="avatar" label="Profile Picture" s3={props.s3} />
          </Col>
        </Row>
        <AsyncButton primary loadingText="Updating..." border>
          Submit
        </AsyncButton>
      </Form>
    </Formik>
  );
}
