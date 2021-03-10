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
import useSettingsState, {selectSettingsState} from "~/logic/state/settings";
import GlobalApi from "~/logic/api/global";
import {AsyncButton} from "~/views/components/AsyncButton";

interface FormSchema {
  hideAvatars: boolean;
  hideNicknames: boolean;
  hideUnreads: boolean;
  hideGroups: boolean;
  hideUtilities: boolean;
  imageShown: boolean;
  audioShown: boolean;
  oembedShown: boolean;
  videoShown: boolean;
}

const settingsSel = selectSettingsState(["calm", "remoteContentPolicy"]);

export function CalmPrefs(props: {
  api: GlobalApi;
}) {
  const { api } = props;
  const {
    calm: {
      hideAvatars,
      hideNicknames,
      hideUnreads,
      hideGroups,
      hideUtilities
    },
    remoteContentPolicy: {
      imageShown,
      videoShown,
      oembedShown,
      audioShown,
    }
  } = useSettingsState(settingsSel);


  const initialValues: FormSchema = {
    hideAvatars,
    hideNicknames,
    hideUnreads,
    hideGroups,
    hideUtilities,
    imageShown,
    videoShown,
    oembedShown,
    audioShown,
  };

  const onSubmit = useCallback(async (v: FormSchema, actions: FormikHelpers<FormSchema>) => {
    await Promise.all([
      api.settings.putEntry('calm', 'hideAvatars', v.hideAvatars),
      api.settings.putEntry('calm', 'hideNicknames', v.hideNicknames),
      api.settings.putEntry('calm', 'hideUnreads', v.hideUnreads),
      api.settings.putEntry('calm', 'hideGroups', v.hideGroups),
      api.settings.putEntry('calm', 'hideUtilities', v.hideUtilities),
      api.settings.putEntry('remoteContentPolicy', 'imageShown', v.imageShown),
      api.settings.putEntry('remoteContentPolicy', 'videoShown', v.videoShown),
      api.settings.putEntry('remoteContentPolicy', 'audioShown', v.audioShown),
      api.settings.putEntry('remoteContentPolicy', 'oembedShown', v.oembedShown),
    ]);
    actions.setStatus({ success: null });
  }, [api]);

  return (
    <Formik initialValues={initialValues} onSubmit={onSubmit}>
      <Form>
        <BackButton/>
        <Col borderBottom="1" borderBottomColor="washedGray" p="5" pt="4" gapY="5">
            <Col gapY="1" mt="0">
            <Text color="black" fontSize={2} fontWeight="medium">
              CalmEngine
            </Text>
            <Text gray>
              Modulate various elements across Landscape to maximize calmness
            </Text>
          </Col>
          <Text fontWeight="medium">Home screen</Text>
          <Toggle
            label="Hide unread counts"
            id="hideUnreads"
            caption="Do not show unread counts on group tiles"
          />
          <Toggle
            label="Hide utility tiles"
            id="hideUtilities"
            caption="Do not show home screen utilities"
          />
          <Toggle
            label="Hide group tiles"
            id="hideGroups"
            caption="Do not show group tiles"
          />
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

          <AsyncButton primary width="fit-content" type="submit">
            Save
          </AsyncButton>
        </Col>
      </Form>
    </Formik>
  );
}
