import { DEFAULT_SESSION } from './constants';
import React, { useCallback } from 'react';
import useTermState, { Session } from './state';
import api from './api';
import { pokeTask } from '@urbit/api/term';

interface TabProps {
  session: Session;
  name: string;
}

export const Tab = ( { session, name }: TabProps ) => {
  const isSelected = useTermState().selected === name;

  const onClick = () => {
    useTermState.getState().set((state) => {
      state.selected = name;
      state.sessions[name].hasBell = false;
    });
  };

  const onDelete = useCallback(async (e) => {
    e.stopPropagation();

    // clean up subscription
    if(session && session.subscriptionId) {
      await api.unsubscribe(session.subscriptionId);
    }

    // DELETE
    await api.poke(pokeTask(name, { shut: null }));

    // remove from zustand
    useTermState.getState().set((state) => {
      if (state.selected === name) {
        state.selected = DEFAULT_SESSION;
      }
      state.names = state.names.filter(n => n !== name);
      delete state.sessions[name];
    });
  }, [session]);

  return (
    <div className={'tab ' + (isSelected ? 'selected' : '')} onClick={onClick}>
      <a className='session-name'>
        {session?.hasBell ? '🔔 ' : ''}
        {name === DEFAULT_SESSION ? 'default' : name}
        {session && session.pending > 0 ? ' o' : ''}
        {' '}
      </a>
      {name === DEFAULT_SESSION ? null : <a className="delete-session" onClick={onDelete}>x</a>}
    </div>
  );
};
