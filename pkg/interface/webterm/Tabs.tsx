import { pokeTask } from '@urbit/api/term';
import api from './api';
import React from 'react';
import useTermState from './state';
import { Tab } from './Tab';

export const Tabs = () => {
  const { sessions, names } = useTermState();

  const onAddClick = () => {
    const name = prompt('please entew a session name uwu');
    if (!name) {
      return;
    }
    //TODO  name must be @ta
    api.poke(pokeTask(name, { open: { term: 'hood', apps: [{ who: '~'+(window as any).ship, app: 'dojo' }] } }));
    useTermState.getState().set(state => {
      state.names = [name, ...state.names].sort();
      state.selected = name;
      state.sessions[name] = null;
    });


  }

  return (
    <div className="tabs">
      { names.map((n, i) => {
        return (
          <Tab session={sessions[n]} name={n} key={i} />
        );
      })}
      <button onClick={onAddClick}>+</button>
    </div>
  );
};
