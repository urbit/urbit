import {
    Col, ManagedToggleSwitchField as Toggle,

    Text
} from '@tlon/indigo-react';
import { putEntry } from '@urbit/api/settings';
import React, { useCallback } from 'react';
import { Form } from 'formik';
import useSettingsState, { SettingsState } from '~/logic/state/settings';
import { BackButton } from './BackButton';
import _ from 'lodash';
import { FormikOnBlur } from '~/views/components/FormikOnBlur';
import airlock from '~/logic/api';

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

const settingsSel = (s: SettingsState): FormSchema => ({
    hideAvatars: s.calm.hideAvatars,
    hideNicknames: s.calm.hideAvatars,
    hideUnreads: s.calm.hideUnreads,
    hideGroups: s.calm.hideGroups,
    hideUtilities: s.calm.hideUtilities,
    imageShown: !s.remoteContentPolicy.imageShown,
    videoShown: !s.remoteContentPolicy.videoShown,
    oembedShown: !s.remoteContentPolicy.oembedShown,
  audioShown: !s.remoteContentPolicy.audioShown
});

export function CalmPrefs() {
  const initialValues = useSettingsState(settingsSel);

  const onSubmit = useCallback(async (v: FormSchema) => {
    _.forEach(v, (bool, key) => {
      const bucket = ['imageShown', 'videoShown', 'audioShown', 'oembedShown'].includes(key) ? 'remoteContentPolicy' : 'calm';
      if(initialValues[key] !== bool) {
        airlock.poke(putEntry(bucket, key, bool));
      }
    });
  }, [initialValues]);

  return (
    <FormikOnBlur initialValues={initialValues} onSubmit={onSubmit}>
      <Form>
        <BackButton />
        <Col borderBottom={1} borderBottomColor="washedGray" p={5} pt={4} gapY={5}>
          <Col gapY={1} mt={0}>
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
          <Text fontWeight="medium">Remote content</Text>
          <Toggle
            label="Disable images"
            id="imageShown"
            caption="Images will be replaced with an inline placeholder that must be clicked to be viewed"
          />
          <Toggle
            label="Disable audio files"
            id="audioShown"
            caption="Audio content will be replaced with an inline placeholder that must be clicked to be viewed"
          />
          <Toggle
            label="Disable video files"
            id="videoShown"
            caption="Video content will be replaced with an inline placeholder that must be clicked to be viewed"
          />
          <Toggle
            label="Disable embedded content"
            id="oembedShown"
            caption="Embedded content may contain scripts that can track you"
          />
        </Col>
      </Form>
    </FormikOnBlur>
  );
}
