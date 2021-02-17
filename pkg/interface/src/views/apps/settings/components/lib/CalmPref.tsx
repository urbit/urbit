import React, {useCallback} from "react";
import {
  Box,
  ManagedToggleSwitchField as Toggle,
  Button,
  Col,
  Text,
} from "@tlon/indigo-react";
import { Formik, Form, FormikHelpers } from "formik";
import * as Yup from "yup";
import { BackButton } from "./BackButton";
import useLocalState, { selectLocalState } from "~/logic/state/local";

interface FormSchema {
  hideAvatars: boolean;
  hideNicknames: boolean;
  imageShown: boolean;
  audioShown: boolean;
  oembedShown: boolean;
}

const localSelector = selectLocalState([
  "hideAvatars",
  "hideNicknames",
  "remoteContentPolicy",
  "set",
]);

export function CalmPrefs(props: {}) {
  const {
    hideAvatars,
    hideNicknames,
    remoteContentPolicy,
    set: setLocalState,
  } = useLocalState(localSelector);
  const {
    imageShown,
    videoShown,
    oembedShown,
    audioShown,
  } = remoteContentPolicy;

  const initialValues: FormSchema = {
    hideAvatars,
    hideNicknames,
    imageShown,
    videoShown,
    oembedShown,
    audioShown,
  };

  const onSubmit = useCallback(async (values: FormSchema, actions: FormikHelpers<FormSchema>) => {
    setLocalState(state => {
      const { hideAvatars, hideNicknames, ...remote } = values;
      Object.assign(state.remoteContentPolicy, remote);
      state.hideNicknames = hideNicknames;
      state.hideAvatars = hideAvatars;
    });

  }, [setLocalState]);

  return (
    <Formik initialValues={initialValues} onSubmit={onSubmit}>
      <Form>
        <Col borderBottom="1" borderBottomColor="washedGray" p="5" gapY="5">
          <BackButton />
          <Col gapY="1">
            <Text color="black" fontSize={2} fontWeight="medium">
              CalmEngine
            </Text>
            <Text gray>
              Modulate various elemednts across Landscape to maximize calmness
            </Text>
          </Col>
          <Text fontWeight="medium">User-set identity</Text>
          <Toggle
            label="Disable avatars"
            id="hideAvatars"
            caption="Do not show user-set avatars"
          />
          <Toggle
            label="Disable nicknames"
            id="hideNicknames"
            caption="Do not show user-set nicknames"
          />
          <Text fontWeight="medium">Remote Content</Text>
          <Toggle
            label="Load images"
            id="imageShown"
            caption="Images will be replaced with an inline placeholder that must be clicked to be viewed"
          />
          <Toggle
            label="Load audio files"
            id="audioShown"
            caption="Audio content will be replaced with an inline placeholder that must be clicked to be viewed"
          />
          <Toggle
            label="Load video files"
            id="videoShown"
            caption="Video content will be replaced with an inline placeholder that must be clicked to be viewed"
          />
          <Toggle
            label="Load embedded content"
            id="oembedShown"
            caption="Embedded content may contain scripts that can track you"
          />

          <Button primary width="fit-content" type="submit">
            Save
          </Button>
        </Col>
      </Form>
    </Formik>
  );
}
