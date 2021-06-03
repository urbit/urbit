import { useEffect, MutableRefObject } from "react";
import { TutorialProgress } from "@urbit/api";
import useLocalState, { selectLocalState } from "~/logic/state/local";

const localSelector = selectLocalState(['tutorialProgress', 'setTutorialRef']);

export function useTutorialModal(
  onProgress: TutorialProgress,
  show: boolean,
  anchorRef: MutableRefObject<HTMLElement | null>
) {
  const { tutorialProgress, setTutorialRef } = useLocalState(localSelector);

  useEffect(() => {
    if (show && (onProgress === tutorialProgress) && anchorRef?.current) {
      setTutorialRef(anchorRef.current);
    }

    return () => {}
  }, [tutorialProgress, show, anchorRef]);

  return show && onProgress === tutorialProgress;
}
