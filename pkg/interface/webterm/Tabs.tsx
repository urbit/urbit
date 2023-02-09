import React from 'react';
import useTermState from './state';
import { Tab } from './Tab';
import { useAddSession } from './lib/useAddSession';
import { Icon } from '@tlon/indigo-react';

export const Tabs = () => {
  const { sessions, names } = useTermState();
  const addSession = useAddSession();

  return (
    <div className="tabs">
      {names.map((n, i) => {
        return (
          <Tab session={sessions[n]} name={n} key={i} />
        );
      })}
      <button className="tab" onClick={addSession}>
        <Icon
          icon="Plus"
          size="18px"
        />
      </button>
    </div>
  );
};
