import { AGENT_SESSION_REGEX, SESSION_ID_REGEX } from '../constants';
import useTermState from '../state';
import api from '../api';
import { pokeTask } from '@urbit/api/term';
import { useCallback } from 'react';

export const useAddSession = () => {
  const { names } = useTermState();

  const addSession = useCallback(async () => {
    let agent = 'hood'; // default agent
    let sessionName: string;

    const userInput = prompt('please entew a session name uwu');
    // user canceled or did not enter a value
    if (!userInput) {
      return;
    }

    // check for custom agent session syntax
    if (AGENT_SESSION_REGEX.test(userInput)) {
      const match = AGENT_SESSION_REGEX.exec(userInput);
      if (!match) {
        return;
      }
      agent = match[1];
      sessionName = match[2];
    // else, use the default session creation regex
    } else if (SESSION_ID_REGEX.test(userInput)) {
      const match = SESSION_ID_REGEX.exec(userInput);
      if (!match) {
        return;
      }
      sessionName = match[1];
    } else {
      return;
    }

    // avoid nil or duplicate sessions
    if(!sessionName || names.includes(sessionName)) {
      return;
    }

    try {
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
