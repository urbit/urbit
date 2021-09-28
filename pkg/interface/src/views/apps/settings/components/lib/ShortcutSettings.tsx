import React, { useCallback, useEffect, useState } from 'react';
import _ from 'lodash';

import { Box, Col, Text } from '@tlon/indigo-react';
import { Formik, Form, useField } from 'formik';
import { putEntry } from '@urbit/api/settings';

import { getChord } from '~/logic/lib/util';
import useSettingsState, {
  selectSettingsState,
  ShortcutMapping
} from '~/logic/state/settings';
import { AsyncButton } from '~/views/components/AsyncButton';
import { BackButton } from './BackButton';
import airlock from '~/logic/api';

const settingsSel = selectSettingsState(['keyboard']);

export function ChordInput(props: { id: string; label: string }) {
  const { id, label } = props;
  const [capturing, setCapturing] = useState(false);
  const [{ value }, , { setValue }] = useField(id);
  const onCapture = useCallback(() => {
    setCapturing(true);
  }, []);
  useEffect(() => {
    if (!capturing) {
      return;
    }
    function onKeydown(e: KeyboardEvent) {
      if (['Control', 'Shift', 'Meta'].includes(e.key)) {
        return;
      }
      const chord = getChord(e);
      setValue(chord);
      e.stopImmediatePropagation();
      e.preventDefault();
      setCapturing(false);
    }
    document.addEventListener('keydown', onKeydown);
    return () => {
      document.removeEventListener('keydown', onKeydown);
    };
  }, [capturing]);

  return (
    <>
      <Box p="1">
        <Text>{label}</Text>
      </Box>
      <Box
        border="1"
        borderColor="lightGray"
        borderRadius="2"
        onClick={onCapture}
        p="1"
      >
        <Text>{capturing ? 'Press' : value}</Text>
      </Box>
    </>
  );
}

export default function ShortcutSettings() {
  const { keyboard } = useSettingsState(settingsSel);

  return (
    <Formik
      initialValues={keyboard}
      onSubmit={async (values: ShortcutMapping, actions) => {
        const promises = _.map(values, (value, key) => {
          return keyboard[key] !== value
            ? airlock.poke(putEntry('keyboard', key, value))
            : Promise.resolve(0);
        });
        await Promise.all(promises);
        actions.setStatus({ success: null });
      }}
    >
      <Form>
        <BackButton />
        <Col p="5" pt="4" gapY="5">
          <Col gapY="1" mt="0">
            <Text color="black" fontSize={2} fontWeight="medium">
              Shortcuts
            </Text>
            <Text gray>Customize keyboard shortcuts for landscape</Text>
          </Col>
          <Box
            display="grid"
            gridTemplateColumns="1fr 100px"
            gridGap={3}
            maxWidth="500px"
          >
            <ChordInput id="navForward" label="Go forward in history" />
            <ChordInput id="navBack" label="Go backward in history" />
            <ChordInput
              id="cycleForward"
              label="Cycle forward through channel list"
            />
            <ChordInput
              id="cycleBack"
              label="Cycle backward through channel list"
            />
            <ChordInput id="hideSidebar" label="Show/hide group sidebar" />
            <ChordInput id="readGroup" label="Read all in a group" />
          </Box>
          <AsyncButton primary width="fit-content">Save Changes</AsyncButton>
        </Col>
      </Form>
    </Formik>
  );
}
