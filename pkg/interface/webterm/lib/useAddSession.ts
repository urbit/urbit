import {
  DEFAULT_HANDLER,
  AGENT_SESSION_REGEX,
  SESSION_ID_REGEX
} from '../constants';
import useTermState from '../state';
import api from '../api';
import { pokeTask } from '@urbit/api/term';
import { useCallback } from 'react';

export const useAddSession = () => {
  const { names } = useTermState();

  const addSession = useCallback(async () => {
    let agent = DEFAULT_HANDLER;
    let sessionName: string;

    const userInput = prompt('Please enter an alpha-numeric session name.');
    // user canceled or did not enter a value
    if (!userInput) {
      alert('A valid name is required to create a new session');
      return;
    }

    // check for custom agent session syntax
    if (AGENT_SESSION_REGEX.test(userInput)) {
      const match = AGENT_SESSION_REGEX.exec(userInput);
      if (!match) {
        alert('Invalid format. Valid syntax: agent!session-name');
        return;
      }
      agent = match[1];
      sessionName = match[2];
    // else, use the default session creation regex
    } else if (SESSION_ID_REGEX.test(userInput)) {
      const match = SESSION_ID_REGEX.exec(userInput);
      if (!match) {
        alert('Invalid format. Valid syntax: session-name');
        return;
      }
      sessionName = match[1];
    } else {
      alert('Invalid format. Valid syntax: session-name');
      return;
    }

    // prevent duplicate sessions
    if(names.includes(sessionName)) {
      alert(`Session name must be unique ("${sessionName}" already in use)`);
      return;
    }

    try {
      //TODO  eventually, customizable app pre-linking?
      await api.poke(pokeTask(sessionName, { open: { term: agent, apps: [{ who: '~' + (window as any).ship, app: 'dojo' }] } }));
      useTermState.getState().set((state) => {
        state.names = [sessionName, ...state.names].sort();
        state.selected = sessionName;
        state.sessions[sessionName] = null;
      });
    } catch (error) {
      console.log('unable to create session:', error);
    }
  }, [names]);

  return {
    addSession
  };
};
