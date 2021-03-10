import { TutorialProgress, Associations } from '@urbit/api';
import { AlignX, AlignY } from '~/logic/lib/relativePosition';
import { Direction } from '~/views/components/Triangle';

export const MODAL_WIDTH = 256;
export const MODAL_HEIGHT = 256;
export const MODAL_WIDTH_PX = `${MODAL_WIDTH}px`;
export const MODAL_HEIGHT_PX = `${MODAL_HEIGHT}px`;

export const TUTORIAL_HOST = process.env.TUTORIAL_HOST!;
export const TUTORIAL_GROUP = process.env.TUTORIAL_GROUP!;
export const TUTORIAL_CHAT = process.env.TUTORIAL_CHAT!;
export const TUTORIAL_BOOK = process.env.TUTORIAL_BOOK!;
export const TUTORIAL_LINKS = process.env.TUTORIAL_LINKS!;
export const TUTORIAL_GROUP_RESOURCE = `/ship/${TUTORIAL_HOST}/${TUTORIAL_GROUP}` ;

interface StepDetail {
  title: string;
  description: string;
  url: string;
  alignX: AlignX | AlignX[];
  alignY: AlignY | AlignY[];
  offsetX: number;
  offsetY: number;
  arrow: Direction;
}

export function hasTutorialGroup(props: { associations: Associations }) {
  return (
    TUTORIAL_GROUP_RESOURCE in props.associations.groups
  );
}

export const getTrianglePosition = (dir: Direction) => {
  const midY = `${MODAL_HEIGHT / 2 - 8}px`;
  const midX = `${MODAL_WIDTH / 2 - 8}px`;
  switch(dir) {
  case 'East':
    return {
      top: midY,
      right: '-32px'
    };
    case 'West':
      return {
        top: midY,
        left: '-32px'
      };
    case 'North':
      return {
        top: '-32px',
        left: midX
      };
    case 'South':
      return {
        bottom: '-32px',
        left: midX
      };
  }
};

export const progressDetails: Record<TutorialProgress, StepDetail> = {
  hidden: {} as any,
  exit: {} as any,
  done: {
    title: 'End',
    description:
      'This tutorial is finished. Would you like to leave Beginner Island?',
    url: '/',
    alignX: 'right',
    alignY: 'top',
    offsetX: MODAL_WIDTH + 8,
    offsetY: 0
  },
  start: {
    title: 'New Group added',
    description:
      'We just added you to the Beginner island group to show you around. This group is public, but other groups can be private',
    url: '/',
    alignX: 'right',
    alignY: 'top',
    arrow: 'West',
    offsetX: MODAL_WIDTH + 24,
    offsetY: 64
  },
  'group-desc': {
    title: 'What\'s a group',
    description:
      'A group contains members and tends to be centered around a topic or multiple topics.',
    url: `/~landscape/ship/${TUTORIAL_HOST}/${TUTORIAL_GROUP}`,
    alignX: 'left',
    alignY: 'top',
    arrow: 'East',
    offsetX: MODAL_WIDTH + 24,
    offsetY: 80,
  },
  channels: {
    title: 'Channels',
    description:
      'Inside a group you have three types of Channels: Chat, Collection, or Notebook. Mix and match these depending on your group context!',
    url: `/~landscape/ship/${TUTORIAL_HOST}/${TUTORIAL_GROUP}`,
    alignY: 'top',
    alignX: 'right',
    arrow: 'West',
    offsetX: MODAL_WIDTH + 24,
    offsetY: -8
  },
  chat: {
    title: 'Chat',
    description:
      'Chat channels are for messaging within your group. Direct Messages can be accessed from Messages in the top right',
    url: `/~landscape/ship/${TUTORIAL_HOST}/${TUTORIAL_GROUP}/resource/chat/ship/${TUTORIAL_HOST}/${TUTORIAL_CHAT}`,
    alignY: 'top',
    arrow: 'North',
    alignX: 'right',
    offsetY: -56,
    offsetX: -8
  },
  link: {
    title: 'Collection',
    description:
      'A collection is where you can share and view links, images, and other media within your group. Every item in a Collection can have it’s own comment thread.',
    url: `/~landscape/ship/${TUTORIAL_HOST}/${TUTORIAL_GROUP}/resource/link/ship/${TUTORIAL_HOST}/${TUTORIAL_LINKS}`,
    alignY: 'top',
    alignX: 'right',
    arrow: 'North',
    offsetX: -8,
    offsetY: -56
  },
  publish: {
    title: 'Notebook',
    description:
      'Notebooks are for creating long-form content within your group. Use markdown to create rich posts with headers, lists and images.',
    url: `/~landscape/ship/${TUTORIAL_HOST}/${TUTORIAL_GROUP}/resource/publish/ship/${TUTORIAL_HOST}/${TUTORIAL_BOOK}`,
    alignY: 'top',
    alignX: 'right',
    arrow: 'North',
    offsetX: -8,
    offsetY: -56
  },
  notifications: {
    title: 'Notifications',
    description: 'You will get updates from subscribed channels and mentions here. You can access Notifications through Leap.',
    url: '/~notifications',
    alignY: 'top',
    alignX: 'left',
    arrow: 'North',
    offsetX: 0,
    offsetY: -48
  },
  profile: {
    title: 'Profile',
    description:
      'Your profile is customizable and can be shared with other ships. Enter as much or as little information as you’d like.',
    url: `/~profile/~${window.ship}`,
    alignY: 'top',
    alignX: 'right',
    arrow: 'South',
    offsetX: -300 + MODAL_WIDTH / 2,
    offsetY: -4,
  },
  leap: {
    title: 'Leap',
    description:
      'Leap allows you to go to a specific channel, message, collection, profile or group simply by typing in a command or selecting a shortcut from the dropdown menu.',
    url: `/~profile/~${window.ship}`,
    alignY: "top",
    alignX: "left",
    arrow: "North",
    offsetX: 76,
    offsetY: -48,
  },
};
