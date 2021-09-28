import React from 'react';
import { UseStore } from 'zustand';
import { BaseState } from '../state/base';

const withStateo = <
  StateType extends BaseState<any>
>(
  useState: UseStore<StateType>,
  Component: any,
  stateMemberKeys?: (keyof StateType)[]
) => {
  return React.forwardRef((props, ref) => {
    const state = stateMemberKeys ? useState(
      state => stateMemberKeys.reduce(
        (object, key) => ({ ...object, [key]: state[key] }), {}
      )
    ) : useState();
    return <Component ref={ref} {...state} {...props} />;
  });
};

interface StatePicker extends Array<any> {
  0: UseStore<any>;
  1?: string[];
}

const withState = (
    Component: any,
    stores: StatePicker[]
) => {
  return React.forwardRef((props, ref) => {
    const stateProps: unknown = {};
    stores.forEach(([store, keys]) => {
      const storeProps = Array.isArray(keys)
        ? store(state => keys.reduce(
          (object, key) => ({ ...object, [key]: state[key] }), {}
        ))
        : store();
      Object.assign(stateProps, storeProps);
    });
    return <Component ref={ref} {...stateProps} {...props} />;
  });
};

export default withState;
