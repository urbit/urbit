import { useEffect } from 'react';

import useLocalState, { selectLocalState } from '~/logic/state/local';
import { TutorialProgress } from '~/types/local-update';

const localSelector = selectLocalState(['tutorialProgress', 'setTutorialRef']);

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
