import React from 'react';
import useStorage, { IuseStorage } from '~/logic/lib/useStorage';

const withStorage = <P, C extends React.ComponentType<P & IuseStorage>>(
  Component: C,
  params = {}
) => {
  return React.forwardRef<C, Omit<C, keyof IuseStorage>>((props, ref) => {
    const storage = useStorage(params);
    // @ts-ignore Error is based on React component attrs?
    return <Component ref={ref} {...storage} {...props} />;
  });
};

export default withStorage;
