import React, { ReactNode, useState, useEffect, useCallback } from "react";
import { useStatelessAsyncClickable } from '~/logic/lib/useStatelessAsyncClickable';

import { Button, LoadingSpinner, Action } from "@tlon/indigo-react";

import { useFormikContext } from "formik";

interface AsyncActionProps {
  children: ReactNode;
  name: string;
  onClick: (e: React.MouseEvent) => Promise<void>;
}

export function StatelessAsyncAction({
  children,
  onClick,
  name = '',
  ...rest
}: AsyncActionProps & Parameters<typeof Action>[0]) {
  const {
    onClick: handleClick,
    buttonState: state,
  } = useStatelessAsyncClickable(onClick, name);

  return (
    <Action onClick={handleClick} {...rest}>
      {state === "error" ? (
        "Error"
      ) : state === "loading" ? (
        <LoadingSpinner
          foreground={rest.destructive ? "red" : "black"}
          background="transparent"
        />
      ) : state === "success" ? (
        "Done"
      ) : (
        children
      )}
    </Action>
  );
}
