import { BaseInput, Box, Col, Text } from '@tlon/indigo-react';
import _ from 'lodash';
import React, { useCallback, useState } from 'react';
import { UseStore } from 'zustand';
import { BaseState } from '~/logic/state/base';
import useContactState from '~/logic/state/contact';
import useGraphState from '~/logic/state/graph';
import useGroupState from '~/logic/state/group';
import useHarkState from '~/logic/state/hark';
import useInviteState from '~/logic/state/invite';
import useLaunchState from '~/logic/state/launch';
import useMetadataState from '~/logic/state/metadata';
import useSettingsState from '~/logic/state/settings';
import useStorageState from '~/logic/state/storage';
import { BackButton } from './BackButton';

interface StoreDebuggerProps {
  name: string;
  useStore: UseStore<BaseState<any> & any>;
}

const objectToString = (obj: any): string => JSON.stringify(obj, null, '  ');

const StoreDebugger = (props: StoreDebuggerProps) => {
  const name = props.name;
  const state = props.useStore();
  const [filter, setFilter] = useState('');
  const [text, setText] = useState(objectToString(state));
  const [visible, setVisible] = useState(false);

  const tryFilter = useCallback((filterToTry) => {
    let output: any = false;
    try {
      output =  _.get(state, filterToTry, undefined);
    } catch (e) {
      console.log('filter failed');
    }
    if (output) {
      console.log(output);
      setText(objectToString(output));
      setFilter(filterToTry);
    }
  }, [state, filter, text]);

  return (
    <Box p={1}>
      <Text cursor="pointer" onClick={() => setVisible(!visible)}>{name}</Text>
      {visible && <Box>
        <BaseInput
          position="sticky"
          top={0}
          my={1}
          p={2}
          backgroundColor='white'
          color='black'
          border='1px solid transparent'
          borderRadius={2}
          fontSize={1}
          placeholder="Drill Down"
          width="100%"
          onKeyUp={(event) => {
            // @ts-ignore clearly value is in eventtarget
            if (event.target.value) {
            // @ts-ignore clearly value is in eventtarget
              tryFilter(event.target.value);
            } else {
              setFilter('');
              setText(objectToString(state));
            }
          }}
        />
        <Text mono p={1} borderRadius={1} display='block' overflow='auto' backgroundColor='washedGray' style={{ whiteSpace: 'pre', wordWrap: 'break-word' }}>{text}</Text>
      </Box>}
    </Box>
  );
};

const DebugPane = () => {
  return (
    <>
      <BackButton />
      <Col borderBottom={1} borderBottomColor="washedGray" p={5} pt={4} gapY={5}>
        <Col gapY={1} mt={0}>
          <Text color="black" fontSize={2} fontWeight="medium">
            Debug Menu
          </Text>
          <Text gray>
            Debug EScape state. Click any state to see its contents and drill down.
          </Text>
        </Col>
        <StoreDebugger name="Contacts" useStore={useContactState} />
        <StoreDebugger name="Graph" useStore={useGraphState} />
        <StoreDebugger name="Group" useStore={useGroupState} />
        <StoreDebugger name="Hark" useStore={useHarkState} />
        <StoreDebugger name="Invite" useStore={useInviteState} />
        <StoreDebugger name="Launch" useStore={useLaunchState} />
        <StoreDebugger name="Metadata" useStore={useMetadataState} />
        <StoreDebugger name="Settings" useStore={useSettingsState} />
        <StoreDebugger name="Storage" useStore={useStorageState} />
      </Col>
    </>
  );
};

export default DebugPane;
