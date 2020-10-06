import React, { ReactNode, useState, useEffect, useCallback } from "react";

import { Button, LoadingSpinner, Action } from "@tlon/indigo-react";

import { useFormikContext } from "formik";

interface AsyncActionProps {
  children: ReactNode;
  onClick: (e: React.MouseEvent) => Promise<void>;
}

type ButtonState = "waiting" | "error" | "loading" | "success";

export function StatelessAsyncAction({
  loadingText,
  children,
  onClick,
  ...rest
}: AsyncActionProps & Parameters<typeof Action>[0]) {
  const [state, setState] = useState<ButtonState>("waiting");
  const handleClick = useCallback(
    async (e: React.MouseEvent) => {
      try {
        setState("loading");
        await onClick(e);
        setState("success");
      } catch (e) {
        console.error(e);
        setState("error");
      } finally {
        setTimeout(() => {
          setState("waiting");
        }, 3000);
      }
    },
    [onClick, setState]
  );

  return (
    <Action onClick={handleClick} {...rest}>
      {state === "error" ? (
        "Error"
      ) : state === "loading" ? (
        <LoadingSpinner
          foreground={rest.primary ? "white" : "black"}
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
