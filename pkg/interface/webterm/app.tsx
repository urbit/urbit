/* eslint-disable max-lines */
import React, {
  useCallback
} from 'react';

import useTermState, { Sessions } from './state';
import { useDark } from './join';
import api from './api';

import { Reset, _dark, _light } from '@tlon/indigo-react';

import 'xterm/css/xterm.css';

import {
  scrySessions
} from '@urbit/api/term';

import { ThemeProvider } from 'styled-components';
import { Tabs } from './Tabs';
import Buffer from './Buffer';

type TermAppProps = {
  ship: string;
}

export default function TermApp(props: TermAppProps) {
  // TODO  allow switching of selected
  const { selected } = useTermState();
  const dark = useDark();

  // TODO: how / where to init the sessions
  const initSessions = useCallback(async () => {
    const response = await api.scry(scrySessions());
    console.log('sessions', response);
    const sessions = response.reduce((memo: Sessions, id) => {
      memo[id] = null;
      return memo;
    }, {} as Sessions);
    console.log('sessions obj', sessions);

    useTermState.getState().set((state) => {
      state.sessions = sessions;
    });
  }, []);

  return (
    <>
      <ThemeProvider theme={dark ? _dark : _light}>
        <Reset />
        <Tabs />aaa
        <Buffer name={selected} />
      </ThemeProvider>
    </>
  );
}
