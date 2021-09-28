import { MutableRefObject, useEffect } from 'react';
import useLocalState, { selectLocalState } from '~/logic/state/local';
import { TutorialProgress } from '~/types';

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

    return () => {};
  }, [tutorialProgress, show, anchorRef]);

  return show && onProgress === tutorialProgress;
}
