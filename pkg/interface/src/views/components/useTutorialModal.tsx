import { useEffect } from "react";
import { TutorialProgress } from "~/types";
import useLocalState, { selectLocalState } from "~/logic/state/local";

const localSelector = selectLocalState(["tutorialProgress", "setTutorialRef"]);

export function useTutorialModal(
  onProgress: TutorialProgress,
  show: boolean,
  anchorRef: HTMLElement | null
) {
  const { tutorialProgress, setTutorialRef } = useLocalState(localSelector);

  useEffect(() => {
    if (show && onProgress === tutorialProgress && anchorRef) {
      setTutorialRef(anchorRef);
    }
  }, [onProgress, tutorialProgress, show, anchorRef]);

  return show && onProgress === tutorialProgress;
}
