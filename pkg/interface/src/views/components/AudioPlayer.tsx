import React, { useCallback, useEffect, useRef, useState } from 'react';
import { Action, Text, Icon, Row } from '@tlon/indigo-react';

function formatTime(num: number) {
  const minutes = Math.floor(num / 60);
  const seconds = Math.floor(num % 60);
  return `${minutes}:${seconds.toString().padStart(2, '0')}`;
}

export function AudioPlayer(props: { url: string; title?: string }) {
  const { url, title = '' } = props;
  const ref = useRef<HTMLAudioElement>();

  const [playing, setPlaying] = useState(false);

  const playPause = useCallback(() => {
    if (playing) {
      ref.current.pause();
    } else {
      ref.current.play();
    }
    setPlaying(p => !p);
  }, [ref, playing]);

  const [duration, setDuration] = useState(0);
  const [progress, setProgress] = useState(0);

  useEffect(() => {
    if (playing) {
      // eslint-disable-next-line no-inner-declarations
      function updateProgress() {
        setProgress(ref.current.currentTime);
      }

      const tim = setInterval(updateProgress, 250);

      return () => {
        tim && clearTimeout(tim);
      };
    }
  }, [ref.current, playing]);

  useEffect(() => {
    ref.current.addEventListener('loadedmetadata', () => {
      setDuration(ref.current.duration);
    });
  }, [ref.current]);

  return (
    <Row
      backgroundColor="white"
      alignItems="center"
      justifyContent="center"
      height="100%"
      width="100%"
      position="relative"
    >
      <Row p="2" position="absolute" left="0" top="0">
        <Text lineHeight="tall">{title}</Text>
      </Row>
      <audio ref={ref} src={url} preload="metadata" />
      <Action backgroundColor="white" height="unset" onClick={playPause}>
        <Icon
          height="32px"
          width="32px"
          color="black"
          icon={playing ? 'LargeBullet' : 'TriangleEast'}
        />
      </Action>
      <Row p="2" position="absolute" right="0" bottom="0">
        <Text lineHeight="tall">
          {formatTime(progress)} / {formatTime(duration)}
        </Text>
      </Row>
    </Row>
  );
}
