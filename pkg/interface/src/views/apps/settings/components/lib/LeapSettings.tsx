import React, { useCallback } from "react";
import _ from 'lodash';
import {
  Col,
  Text,
  ManagedToggleSwitchField as Toggle,
  ManagedCheckboxField,
} from "@tlon/indigo-react";
import { Form, FormikHelpers } from "formik";
import { FormikOnBlur } from "~/views/components/FormikOnBlur";
import { BackButton } from "./BackButton";
import GlobalApi from "~/logic/api/global";
import { NotificationGraphConfig, LeapCategories, leapCategories } from "~/types";
import useLocalState, {selectLocalState} from "~/logic/state/local";

type FormSchema = {
  [c in LeapCategories]: boolean;
}

const localSelector = selectLocalState(["hideLeapCats", "set"]);

export function LeapSettings(props: {}) {

  const { hideLeapCats, set: setLocalState } = useLocalState(localSelector);

  const initialValues: FormSchema = leapCategories.reduce((acc, val, key) => {
    return {...acc, [val]: hideLeapCats.findIndex(c => c === val) !== -1 };
  }, {} as FormSchema);

  const onSubmit = useCallback((values: FormSchema) => {
    setLocalState(state => {
      state.hideLeapCats = _.keys(_.pickBy(values, v => v)) as any;
    });

  }, [setLocalState]);
  
  return (
    <Col p="5" gapY="5">
      <BackButton />
      <Col gapY="2">
        <Text fontSize="2" fontWeight="medium">
          Leap
        </Text>
        <Text fontSize="0" gray>
          Customize Leap ordering, omit modules or results
        </Text>
      </Col>
      <FormikOnBlur initialValues={initialValues} onSubmit={onSubmit}>
        <Form>
          <Col gapY="4">
            <Text fontWeight="medium" fontSize="0">
              Customize default Leap sections
            </Text>
            <ManagedCheckboxField id="mychannel" label="My Channel" />
            <ManagedCheckboxField id="updates" label="Updates Inbox" />
            <ManagedCheckboxField id="profile" label="Profile and Settings" />
            <ManagedCheckboxField id="messages" label="Messages" />
            <ManagedCheckboxField id="logout" label="Log Out" />
          </Col>
        </Form>
      </FormikOnBlur>
    </Col>
  );
}
