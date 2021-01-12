import React from 'react';

import {
  Box,
  ManagedCheckboxField as Checkbox,
  Button
} from '@tlon/indigo-react';
import { Formik, Form } from 'formik';
import * as Yup from 'yup';

import GlobalApi from '~/logic/api/global';
import { uxToHex } from '~/logic/lib/util';
import { S3State, BackgroundConfig } from '~/types';
import { BackgroundPicker, BgType } from './BackgroundPicker';
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
          <Box
            display="grid"
            gridTemplateColumns="100%"
            gridTemplateRows="auto"
            gridRowGap={5}
          >
            <Box color="black" fontSize={1} mb={3} fontWeight={900}>
              Display Preferences
            </Box>
            <BackgroundPicker
              bgType={props.values.bgType}
              bgUrl={props.values.bgUrl}
              api={api}
              s3={s3}
            />
            <Checkbox
              label="Disable avatars"
              id="avatars"
              caption="Do not show user-set avatars"
            />
            <Checkbox
              label="Disable nicknames"
              id="nicknames"
              caption="Do not show user-set nicknames"
            />
            <Button border={1} style={{ cursor: 'pointer' }} borderColor="washedGray" type="submit">
              Save
            </Button>
          </Box>
        </Form>
      )}
    </Formik>
  );
}
