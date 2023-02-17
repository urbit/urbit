import useIsMounted from './lib/useIsMounted';
import React from 'react';
import { useEffect, useState } from 'react';

const DELAY_MS = 1000;
const FRAME_MS = 250;
const CHARS = '|/-\\';

const Spinner = () => {
  const [index, setIndex] = useState(0);
  const [intervalTimer, setIntervalTimer] = useState<ReturnType<typeof setInterval> | undefined>();
  const isMounted = useIsMounted();

  useEffect(() => {
    setIntervalTimer(
      setInterval(() => {
        if (isMounted()) {
          setIndex(idx => idx === CHARS.length - 1 ? 0 : idx + 1);
        }
      }, FRAME_MS)
    );

    return () => {
      if (intervalTimer) {
        clearInterval(intervalTimer);
      }
    };
  }, []);

  return <span>&nbsp;{CHARS[index]}</span>;
};

export const DelayedSpinner = () => {
  const [showSpinner, setShowSpinner] = useState(false);
  const [delayTimer, setDelayTimer] = useState<ReturnType<typeof setTimeout> | undefined>();
  const isMounted = useIsMounted();

  useEffect(() => {
    setDelayTimer(
      setTimeout(() => {
        if (isMounted()) {
          setShowSpinner(true);
        }
      }, DELAY_MS)
    );

    return () => {
      if (delayTimer) {
        clearTimeout(delayTimer);
      }
    };
  }, []);

  return showSpinner ? <Spinner /> : null;
};
