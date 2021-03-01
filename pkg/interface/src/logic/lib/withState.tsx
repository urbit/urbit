import React from "react";
import { ReactElement } from "react";
import { UseStore } from "zustand";
import { BaseState } from "../state/base";

const withState = <
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
    return <Component ref={ref} {...state} {...props} />
  })
};

export default withState;