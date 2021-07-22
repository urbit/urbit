import {
  Col,
  Label,
  ManagedRadioButtonField as Radio,
  Text
} from '@tlon/indigo-react';
import { Form } from 'formik';
import React, { useCallback } from 'react';
import * as Yup from 'yup';
import { uxToHex } from '~/logic/lib/util';
import useSettingsState, { SettingsState } from '~/logic/state/settings';
import { FormikOnBlur } from '~/views/components/FormikOnBlur';
import { BackButton } from './BackButton';
import { BackgroundPicker, BgType } from './BackgroundPicker';
import shallow from 'zustand/shallow';

const formSchema = Yup.object().shape({
  bgType: Yup.string()
    .oneOf(['none', 'color', 'url'], 'invalid')
    .required('Required'),
  bgColor: Yup.string().when('bgType', (bgType, schema) => bgType === 'color' ? schema.required() : schema),
  bgUrl: Yup.string().when('bgType', (bgType, schema) => bgType === 'url' ? schema.required() : schema),
  theme: Yup.string().oneOf(['light', 'dark', 'auto']).required('Required')
});

interface FormSchema {
  bgType: BgType;
  bgColor: string | undefined;
  bgUrl: string | undefined;
  theme: string;
}
const emptyString = '';

const settingsSel = (s: SettingsState): FormSchema => {
  const { display } = s;
  let bgColor = emptyString;
  let bgUrl = emptyString;
  if (display.backgroundType === 'url') {
    bgUrl = display.background;
  }
  if (display.backgroundType === 'color') {
    bgColor = display.background;
  }
  return {
    bgType: display.backgroundType,
    bgColor,
    bgUrl,
    theme: display.theme
  };
};

export default function DisplayForm() {
  const initialValues = useSettingsState(settingsSel, shallow);

  const onSubmit = useCallback(async (values) => {
    const { putEntry } = useSettingsState.getState();
    putEntry('display', 'backgroundType', values.bgType);
    putEntry(
      'display',
      'background',
      values.bgType === 'color'
        ? `#${uxToHex(values.bgColor || '0x0')}`
        : values.bgType === 'url'
        ? values.bgUrl || ''
        : false
    );
    putEntry('display', 'theme', values.theme);
  }, []);

  return (
    <FormikOnBlur
      validationSchema={formSchema}
      initialValues={initialValues}
      onSubmit={onSubmit}
    >
      <Form>
        <BackButton />
        <Col p={5} pt={4} gapY={5}>
          <Col overflowY="auto" gapY={1} mt={0}>
            <Text color="black" fontSize={2} fontWeight="medium">
              Display Preferences
            </Text>
            <Text gray>Customize visual interfaces across your Landscape</Text>
          </Col>
          <BackgroundPicker />
          <Label>Theme</Label>
          <Radio name="theme" id="light" label="Light" />
          <Radio name="theme" id="dark" label="Dark" />
          <Radio name="theme" id="auto" label="Auto" />
        </Col>
      </Form>
    </FormikOnBlur>
  );
}
