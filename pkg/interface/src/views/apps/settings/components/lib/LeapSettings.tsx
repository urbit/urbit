import {
    Col,

    ManagedCheckboxField, Text
} from '@tlon/indigo-react';
import { Form, useFormikContext } from 'formik';
import _ from 'lodash';
import React from 'react';
import useSettingsState, { selectSettingsState } from '~/logic/state/settings';
import {
    LeapCategories,
    leapCategories
} from '~/types';
import { FormikOnBlur } from '~/views/components/FormikOnBlur';
import { ShuffleFields } from '~/views/components/ShuffleFields';
import { BackButton } from './BackButton';

const labels: Record<LeapCategories, string> = {
  updates: 'Notifications',
  profile: 'Profile',
  messages: 'Messages',
  logout: 'Log Out'
};

interface FormSchema {
  categories: { display: boolean; category: LeapCategories }[];
}

function CategoryCheckbox(props: { index: number }) {
  const { index } = props;
  const { values } = useFormikContext<FormSchema>();
  const cats = values.categories;

  const { category } = cats[index];
  const label = labels[category];

  return (
    <ManagedCheckboxField id={`categories[${index}].display`} label={label} />
  );
}

const settingsSel = selectSettingsState(['leap', 'set']);

export function LeapSettings() {
  const { leap } = useSettingsState(settingsSel);
  const categories = leap.categories as LeapCategories[];
  const missing = _.difference(leapCategories, categories);

  const initialValues = {
    categories: [
      ...categories.map(cat => ({
        category: cat,
        display: true
      })),
      ...missing.map(cat => ({ category: cat, display: false }))
    ]
  };

  const onSubmit = async (values: FormSchema) => {
    const { putEntry } = useSettingsState.getState();
    const result = values.categories.reduce(
      (acc, { display, category }) => (display ? [...acc, category] : acc),
      [] as LeapCategories[]
    );
    await putEntry('leap', 'categories', result);
  };

  return (
    <>
    <BackButton />
    <Col p={5} pt={4} gapY={5}>
      <Col gapY={1} mt={0}>
        <Text fontSize={2} fontWeight="medium">
          Leap
        </Text>
        <Text gray>
          Customize Leap ordering, omit modules or results
        </Text>
      </Col>
      <FormikOnBlur initialValues={initialValues} onSubmit={onSubmit}>
        <Form>
          <Col gapY={4}>
            <Text fontWeight="medium">
              Customize default Leap sections
            </Text>
            <ShuffleFields name="categories">
              {(index, helpers) => <CategoryCheckbox index={index} />}
            </ShuffleFields>
          </Col>
        </Form>
      </FormikOnBlur>
    </Col>
    </>
  );
}
