import React, { ReactNode, useState, useEffect, useCallback } from "react";

import { Button, LoadingSpinner } from "@tlon/indigo-react";
import { useFormikContext } from "formik";

import { useStatelessAsyncClickable } from "~/logic/lib/useStatelessAsyncClickable";

interface AsyncButtonProps {
  children: ReactNode;
  name: string;
  onClick: (e: React.MouseEvent) => Promise<void>;
}

export function StatelessAsyncButton({
  children,
  onClick,
  name = "",
  ...rest
}: AsyncButtonProps & Parameters<typeof Button>[0]) {
  const {
    onClick: handleClick,
    buttonState: state,
  } = useStatelessAsyncClickable(onClick, name);

  return (
    <Button onClick={handleClick} {...rest}>
      {state === "error" ? (
        "Error"
      ) : state === "loading" ? (
        <LoadingSpinner
          foreground={
            rest.primary ? "white" : rest.destructive ? "red" : "black"
          }
          background="gray"
        />
      ) : state === "success" ? (
        "Done"
      ) : (
        children
      )}
    </Button>
  );
}
