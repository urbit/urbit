import React from 'react';

import {
  Box,
  ManagedCheckboxField as Checkbox,
  Button,
  Col,
  Text
} from '@tlon/indigo-react';
import { Formik, Form } from 'formik';
import * as Yup from 'yup';

import GlobalApi from '~/logic/api/global';
import { uxToHex } from '~/logic/lib/util';
import { S3State, BackgroundConfig } from '~/types';
import { BackgroundPicker, BgType } from './BackgroundPicker';
import { BackButton } from "./BackButton";
import useLocalState, { LocalState } from '~/logic/state/local';

const formSchema = Yup.object().shape({
  bgType: Yup.string()
    .oneOf(['none', 'color', 'url'], 'invalid')
    .required('Required'),
  bgUrl: Yup.string().url(),
  bgColor: Yup.string(),
  avatars: Yup.boolean(),
  nicknames: Yup.boolean()
});

interface FormSchema {
  bgType: BgType;
  bgColor: string | undefined;
  bgUrl: string | undefined;
  avatars: boolean;
  nicknames: boolean;
}

interface DisplayFormProps {
  api: GlobalApi;
  s3: S3State;
}

export default function DisplayForm(props: DisplayFormProps) {
  const { api, s3 } = props;

  const { hideAvatars, hideNicknames, background, set: setLocalState } = useLocalState();

  let bgColor, bgUrl;
  if (background?.type === 'url') {
    bgUrl = background.url;
  }
  if (background?.type === 'color') {
    bgColor = background.color;
  }
  const bgType = background?.type || 'none';

  return (
    <Formik
      validationSchema={formSchema}
      initialValues={
        {
          bgType,
          bgColor: bgColor || '',
          bgUrl,
          avatars: hideAvatars,
          nicknames: hideNicknames
        } as FormSchema
      }
      onSubmit={(values, actions) => {
        const bgConfig: BackgroundConfig =
          values.bgType === 'color'
            ? { type: 'color', color: `#${uxToHex(values.bgColor || '0x0')}` }
            : values.bgType === 'url'
            ? { type: 'url', url: values.bgUrl || '' }
            : undefined;

        setLocalState((state: LocalState) => {
          state.background = bgConfig;
          state.hideAvatars = values.avatars;
          state.hideNicknames = values.nicknames;
        });
        actions.setSubmitting(false);
      }}
    >
      {props => (
        <Form>
          <Col p="5" gapY="5">
            <BackButton />
            <Col gapY="1">
              <Text color="black" fontSize={2} fontWeight="medium">
                Display Preferences
              </Text>
              <Text gray>
                Customize visual interfaces across your Landscape
              </Text>
            </Col>
            <BackgroundPicker
              bgType={props.values.bgType}
              bgUrl={props.values.bgUrl}
              api={api}
              s3={s3}
            />
            <Button primary width="fit-content" type="submit">
              Save
            </Button>
          </Col>
        </Form>
      )}
    </Formik>
  );
}
