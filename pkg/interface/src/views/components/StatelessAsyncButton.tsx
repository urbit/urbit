import React, { ReactNode, useState, useEffect, useCallback } from "react";

import { Button, LoadingSpinner } from "@tlon/indigo-react";

import { useFormikContext } from "formik";

interface AsyncButtonProps {
  children: ReactNode;
  onClick: (e: React.MouseEvent) => Promise<void>;
}

type ButtonState = "waiting" | "error" | "loading" | "success";

export function StatelessAsyncButton({
  loadingText,
  children,
  onClick,
  ...rest
}: AsyncButtonProps & Parameters<typeof Button>[0]) {
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
    <Button onClick={handleClick} {...rest}>
      {state === "error" ? (
        "Error"
      ) : state === "loading" ? (
        <LoadingSpinner
          foreground={
            rest.primary ? "white" : rest.destructive ? "red" : "black"
          }
          background="transparent"
        />
      ) : state === "success" ? (
        "Done"
      ) : (
        children
      )}
    </Button>
  );
}
