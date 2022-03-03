import React, {
  useCallback, useEffect
} from 'react';

import useTermState from './state';
import { useDark } from './lib/useDark';
import api from './api';

import { _dark, _light } from '@tlon/indigo-react';

import 'xterm/css/xterm.css';

import {
  scrySessions
} from '@urbit/api/term';

import { ThemeProvider } from 'styled-components';
import { Tabs } from './Tabs';
import Buffer from './Buffer';
import { DEFAULT_SESSION } from './constants';
import { showSlog } from './lib/blit';

type TermAppProps = {
  ship: string;
}

export default function TermApp(props: TermAppProps) {
  const { names, selected } = useTermState();
  const dark = useDark();

  const initSessions = useCallback(async () => {
    const response = await api.scry(scrySessions());

    useTermState.getState().set((state) => {
      state.names = response.sort();
    });
  }, []);

  const setupSlog = useCallback(() => {
    console.log('slog: setting up...');
    let available = false;
    const slog = new EventSource('/~_~/slog', { withCredentials: true });

    slog.onopen = (e) => {
      console.log('slog: opened stream');
      available = true;
    };

    slog.onmessage = (e) => {
      const session = useTermState.getState().sessions[DEFAULT_SESSION];
      if (!session) {
        console.log('slog: default session mia!', 'msg:', e.data);
        console.log(Object.keys(useTermState.getState().sessions), session);
        return;
      }
      showSlog(session.term, e.data);
    };

    slog.onerror = (e) => {
      console.error('slog: eventsource error:', e);
      if (available) {
        window.setTimeout(() => {
          if (slog.readyState !== EventSource.CLOSED) {
            return;
          }
          console.log('slog: reconnecting...');
          setupSlog();
        }, 10000);
      }
    };

    useTermState.getState().set((state) => {
      state.slogstream = slog;
    });
  }, []);

  useEffect(() => {
    initSessions();
    setupSlog();
  }, []);

  return (
    <>
      <ThemeProvider theme={dark ? _dark : _light}>
        <Tabs />
        <div className="buffer-container">
          {names.map(name => {
            return <Buffer name={name} selected={name === selected} dark={dark}/>;
          })}
        </div>
      </ThemeProvider>
    </>
  );
}
