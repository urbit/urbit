import { DEFAULT_SESSION } from './constants';
import React, { useCallback, useEffect } from 'react';
import useTermState, { Session } from './state';
import api from './api';
import { pokeTask } from '@urbit/api/term';
import { DelayedSpinner as Spinner } from './Spinner';

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

  // TODO: sometimes the pending is not decremented?
  useEffect(() => {
    if(session) {
      console.log(`${session.subscriptionId}: ${session.pending} pending`);
    }
  }, [session]);

  return (
    <div className={'tab ' + (isSelected ? 'selected' : '')} onClick={onClick}>
      <a className='session-name'>
        {session?.hasBell ? '🔔 ' : ''}
        {name === DEFAULT_SESSION ? 'default' : name}
        {session && session.pending > 0 ? <Spinner /> : null}
        {' '}
      </a>
      {name === DEFAULT_SESSION ? null : <a className="delete-session" onClick={onDelete}>x</a>}
    </div>
  );
};
