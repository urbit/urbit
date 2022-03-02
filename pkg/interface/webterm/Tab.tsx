import { DEFAULT_SESSION } from './constants';
import React, { useCallback } from 'react';
import useTermState from './state';
import { style } from 'styled-system';
import api from './api';
import { pokeTask } from '@urbit/api/term';

export const Tab = ( { session, name } ) => {

  const isSelected = useTermState().selected === name;

  const onClick = () => {
    console.log('click!', name);
    useTermState.getState().set((state) => {
      state.selected = name;
      state.sessions[name].hasBell = false;
    });
    useTermState.getState().sessions[name]?.term?.focus();
  }

  const onDelete = (e) => {
    e.stopPropagation();
    api.poke(pokeTask(name, { shut: null }));
    useTermState.getState().set(state => {
      if (state.selected === name) {
        state.selected = DEFAULT_SESSION;
      }
      state.names = state.names.filter(n => n !== name);
      delete state.sessions[name];
    });
    //TODO  clean up the subscription
  }

  return (
    <div className={'tab ' + isSelected ? 'selected' : ''}>
      <a className='session-name' onClick={onClick}>
        {session?.hasBell ? '🔔 ' : ''}
        {name === DEFAULT_SESSION ? 'default' : name}
        {' '}
      </a>
      {name === DEFAULT_SESSION ? null : <a className="delete-session" onClick={onDelete}>x</a>}
    </div>
  );
};
