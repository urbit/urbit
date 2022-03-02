import React from 'react';
import useTermState from './state';
import { Tab } from './Tab';

export const Tabs = () => {
  const { sessions } = useTermState();
  const sessionKeys = Object.keys(sessions).sort();

  return (
    <>
      { sessionKeys.map((k, i) => {
        return (
          <>
            <Tab session={sessions[k]} name={k} key={i} />
          </>
        );
      })}
    </>
  );
};
