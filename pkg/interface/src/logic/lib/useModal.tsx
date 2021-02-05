import React, {
  useState,
  ReactNode,
  useCallback,
  SyntheticEvent,
  useMemo,
  useEffect,
  useRef,
} from "react";

import { Box } from "@tlon/indigo-react";
import { useOutsideClick } from "./useOutsideClick";
import { ModalOverlay } from "~/views/components/ModalOverlay";

type ModalFunc = (dismiss: () => void) => JSX.Element;
interface UseModalProps {
  modal: JSX.Element | ModalFunc;
}

interface UseModalResult {
  modal: ReactNode;
  showModal: () => void;
}

export function useModal(props: UseModalProps): UseModalResult {
  const innerRef = useRef<HTMLElement>();
  const [modalShown, setModalShown] = useState(false);

  const dismiss = useCallback(() => {
    setModalShown(false);
  }, []);

  const showModal = useCallback(() => {
    setModalShown(true);
  }, []);

  const inner = useMemo(
    () =>
      !modalShown
        ? null
        : typeof props.modal === "function"
        ? props.modal(dismiss)
        : props.modal,
    [modalShown, props.modal, dismiss]
  );

  useOutsideClick(innerRef, dismiss);

  const modal = useMemo(
    () =>
      !inner ? null : (
        <ModalOverlay
          ref={innerRef}
          maxWidth="500px"
          width="100%"
          bg="white"
          borderRadius={2}
          border={[0, 1]}
          borderColor={["washedGray", "washedGray"]}
          display="flex"
          alignItems="stretch"
          flexDirection="column"
        >
          {inner}
        </ModalOverlay>
      ),
    [inner, dismiss]
  );

  return {
    showModal,
    modal,
  };
}
