import { TutorialProgress, Associations } from "~/types";
import {
  AlignX,
  AlignY,
} from "~/logic/lib/relativePosition";

export const MODAL_WIDTH = 256;
export const MODAL_HEIGHT = 180;
export const MODAL_WIDTH_PX = `${MODAL_WIDTH}px`;
export const MODAL_HEIGHT_PX = `${MODAL_HEIGHT}px`;

interface StepDetail {
  title: string;
  description: string;
  url: string;
  alignX: AlignX | AlignX[];
  alignY: AlignY | AlignY[];
  offsetX: number;
  offsetY: number;
}

export function hasTutorialGroup(props: { associations: Associations }) {
  return '/ship/~hastuc-dibtux/beginner-island' in props.associations.groups;
}

export const progressDetails: Record<TutorialProgress, StepDetail> = {
  hidden: {} as any,
  done: {
    title: "End",
    description: "This tutorial is finished. Would you like to leave Beginner Island?",
    url: "/",
    alignX: "right",
    alignY: "top",
    offsetX: MODAL_WIDTH + 8,
    offsetY: 0
  },
  start: {
    title: "New group added",
    description:
      "We just added you to the Beginner island group to show you around. This group is public, but other groups can be private",
    url: "/",
    alignX: "right",
    alignY: "top",
    offsetX: MODAL_WIDTH + 8,
    offsetY: 0
  },
  'group-desc': {
    title: "What's a group",
    description: "A group contains members and tends to be centered around a topic or multiple topics.",
    url: "/~landscape/ship/~hastuc-dibtux/beginner-island",
    alignX: "left",
    alignY: "top",
    offsetX: MODAL_WIDTH + 8,
    offsetY: (MODAL_HEIGHT / 2) - 8,
  },
  channels: {
    title: "Channels",
    description: "Inside a group you have three types of Channels: Chat, Collection, or Notebook. Mix and match these depending on your group context!",
    url: "/~landscape/ship/~hastuc-dibtux/beginner-island",
    alignY: "top",
    alignX: "right",
    offsetX: MODAL_WIDTH + 8,
    offsetY: -8,
  },
  chat: {
    title: "Chat",
    description: "Chat channels are for messaging within your group. Direct Messages are also supported, and are accessible from the “DMs” tile on the homescreen",
    url: "/~landscape/ship/~hastuc-dibtux/beginner-island/resource/chat/ship/~hastuc-dibtux/chat-8401",
    alignY: "top",
    alignX: "right",
    offsetX: 0,
    offsetY: -32,
  },
  link: {
    title: "Collection",
    description: "A collection is where you can share and view links, images, and other media within your group. Every item in a Collection can have it’s own comment thread.",
    url: "/~landscape/ship/~hastuc-dibtux/beginner-island/resource/link/ship/~hastuc-dibtux/link-4353",
    alignY: "top",
    alignX: "right",
    offsetX: 0,
    offsetY: -32,
  },
  publish: {
    title: "Notebook",
    description: "Notebooks are for creating long-form content within your group. Use markdown to create rich posts with headers, lists and images.", 
    url: "/~landscape/ship/~hastuc-dibtux/beginner-island/resource/publish/ship/~hastuc-dibtux/notebook-9148",
    alignY: "top",
    alignX: "right",
    offsetX: 0,
    offsetY: -32,
  },
  notifications: {
    title: "Notifications",
    description: "Subscribing to a channel will send you notifications when there are new updates. You will also receive a notification when someone mentions your name in a channel.", 
    url: "/~landscape/ship/~hastuc-dibtux/beginner-island/resource/publish/ship/~hastuc-dibtux/notebook-9148/settings#notifications",
    alignY: "top",
    alignX: "right",
    offsetX: 0,
    offsetY: -32,
  },
  profile: {
    title: "Profile",
    description: "Your profile is customizable and can be shared with other ships. Enter as much or as little information as you’d like.",
    url: `/~profile/~${window.ship}`,
    alignY: "top",
    alignX: "right",
    offsetX: -300 + (MODAL_WIDTH / 2),
    offsetY: -120 + (MODAL_HEIGHT / 2),
  },
  leap: {
    title: "Leap",
    description: "Leap allows you to go to a specific channel, message, collection, profile or group simply by typing in a command or selecting a shortcut from the dropdown menu.",
    url: `/~profile/~${window.ship}`,
    alignY: "top",
    alignX: "left",
    offsetX: 0,
    offsetY: -32,
  }
};

