import {
  Col,
  Label,
  ManagedRadioButtonField as Radio,
  Text
} from '@tlon/indigo-react';
import { Form } from 'formik';
import { putEntry } from '@urbit/api/settings';
import React, { useMemo } from 'react';
import * as Yup from 'yup';
import { uxToHex } from '~/logic/lib/util';
import useSettingsState, { selectSettingsState } from '~/logic/state/settings';
import { FormikOnBlur } from '~/views/components/FormikOnBlur';
import { BackButton } from './BackButton';
import airlock from '~/logic/api';
import { BackgroundPicker, BgType } from './BackgroundPicker';

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

const settingsSel = selectSettingsState(['display']);

export default function DisplayForm() {
  const {
    display: { background, backgroundType, theme }
  } = useSettingsState(settingsSel);

  const initialValues: FormSchema = useMemo(() => {
    let bgColor, bgUrl;
    if (backgroundType === 'url') {
      bgUrl = background;
    }
    if (backgroundType === 'color') {
      bgColor = background;
    }
    return {
      bgType: backgroundType,
      bgColor: bgColor || '',
      bgUrl,
      theme
    };
  }, [backgroundType, background, theme]);

  return (
    <FormikOnBlur
      validationSchema={formSchema}
      initialValues={initialValues}
      onSubmit={async (values, actions) => {
        const promises = [] as Promise<any>[];
        promises.push(
          airlock.poke(putEntry('display', 'backgroundType', values.bgType))
        );
        promises.push(
          airlock.poke(
            putEntry(
              'display',
              'background',
              values.bgType === 'color'
                ? `#${uxToHex(values.bgColor || '0x0')}`
                : values.bgType === 'url'
                ? values.bgUrl || ''
                : false
            )
          )
        );
        promises.push(airlock.poke(putEntry('display', 'theme', values.theme)));
      }}
    >
      <Form>
        <BackButton />
        <Col p={5} pt={4} gapY={5}>
          <Col gapY={1} mt={0}>
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
