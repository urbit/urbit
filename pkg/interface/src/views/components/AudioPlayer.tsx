import React, { useCallback } from 'react';
import { Action, Text, Icon, Row } from '@tlon/indigo-react';
import useAudioState, {
  AudioState,
  useCurrentTrack,
  useFormattedSeek,
  useTransport
} from '~/logic/state/audio';
import { TruncatedText } from './TruncatedText';

const selChange = (s: AudioState) => s.changeTrack;

export function AudioPlayer(props: { url: string; title?: string }) {
  const { url, title = '' } = props;
  const transport = useTransport();
  const { progress, duration } = useFormattedSeek();
  const current = useCurrentTrack();
  const changeTrack = useAudioState(selChange);

  const isCurrTrack = current?.url === url;
  const playPause = useCallback((e: React.MouseEvent) => {
    e.stopPropagation();
    if (isCurrTrack) {
      transport.playing ? transport.pause() : transport.play();
    } else {
      changeTrack({ url, title });
    }
  }, [isCurrTrack, transport, changeTrack, url, title]);

  return (
    <Row
      backgroundColor="white"
      alignItems="center"
      justifyContent="center"
      height="100%"
      width="100%"
      position="relative"
    >
      <Row width="100%" p="2" position="absolute" left="0" top="0">
        <TruncatedText lineHeight="tall">{title}</TruncatedText>
      </Row>
      <Action backgroundColor="white" height="unset" onClick={playPause}>
        <Icon
          height="32px"
          width="32px"
          color="black"
          icon={
            isCurrTrack && transport.playing ? 'LargeBullet' : 'TriangleEast'
          }
        />
      </Action>
      {isCurrTrack && (
        <Row p="2" position="absolute" right="0" bottom="0">
          <Text lineHeight="tall">
            <Text gray>{progress}</Text>
            <Text>/{duration}</Text>
          </Text>
        </Row>
      )}
    </Row>
  );
}
