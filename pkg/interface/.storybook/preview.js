import React from "react";
import dark from "@tlon/indigo-dark";
import light from "@tlon/indigo-light";
import { Reset } from "@tlon/indigo-react";
import { BrowserRouter } from "react-router-dom";
import { ThemeProvider } from "styled-components";
import useGraphState from "~/logic/state/graph";
import useGroupState from "~/logic/state/group";
import useMetadataState from "~/logic/state/metadata";
import useContactState from "~/logic/state/contact";
import "~/views/landscape/css/custom.css";
import "~/views/css/fonts.css";
import "~/views/apps/chat/css/custom.css";
import "~/views/css/indigo-static.css";

export const parameters = {
  actions: { argTypesRegex: "^on[A-Z].*" },
  controls: {
    matchers: {
      color: /(background|color)$/i,
      date: /Date$/,
    },
  },
};

const groupPreview = {
  group: "/ship/~bollug-worlus/urbit-index",
  channels: {
    "/ship/~darrux-landes/index-weekly": {
      metadata: {
        preview: false,
        vip: "",
        title: "Index Weekly",
        description: "A weekly roundup of content from around the network",
        creator: "~bollug-worlus",
        picture: "",
        hidden: false,
        config: {
          graph: "publish",
        },
        "date-created": "~2020.4.6..21.53.30..dc68",
        color: "0x0",
      },
      "app-name": "graph",
      resource: "/ship/~bollug-worlus/index-weekly",
      group: "/ship/~bollug-worlus/urbit-index",
    },
  },
  members: 1237,
  "channel-count": 3,
  metadata: {
    preview: false,
    vip: "",
    title: "Urbit Index",
    description: "A weekly roundup of content form around the network",
    creator: "~bollug-worlus",
    picture: "",
    hidden: false,
    config: {
      group: null,
    },
    "date-created": "~2020.4.6..21.53.30..dc68",
    color: "0x0",
  },
};

const groupPending = (progress) => ({
  hidden: false,
  started: Date.now() - 3600,
  ship: "~bollug-worlus",
  progress,
  shareContact: false,
  autojoin: false,
  app: "groups",
  invite: [],
});

export const globalTypes = {
  theme: {
    name: "Theme",
    description: "Global Theme for components",
    defaultValue: "light",
    toolbar: {
      icon: "circlehollow",
      items: ["light", "dark"],
    },
  },
};

export const decorators = [
  (Story, context) => {
    window.ship = "sampel-palnet";
    const theme = context.globals.theme === "light" ? light : dark;

    useContactState.setState({
      contacts: {
        "~ridlur-figbud": {
          status: "please like and subscribe",
          "last-updated": 1616609090555,
          avatar: null,
          cover: null,
          bio: "",
          nickname: "Gav",
          color: "0x26.3e0f",
          groups: [],
        },
        "~sampel-palnet": {
          status: "A test status",
          "last-updated": 1616609090555,
          avatar: null,
          cover: null,
          bio: "",
          nickname: "You",
          color: "0x26.3e0f",
          groups: [],
        },
      },
    });

    useGroupState.setState({
      pendingJoin: {
        "/ship/~bollug-worlus/urbit-index-start": groupPending("start"),
        "/ship/~bollug-worlus/urbit-index-metadata": groupPending("metadata"),
        "/ship/~bollug-worlus/urbit-index-done": groupPending("done"),
        "/ship/~bollug-worlus/urbit-index-error": groupPending("no-perms"),
      },
    });

    useMetadataState.setState({
      associations: {
        groups: {
          "/ship/~bitbet-bolbel/urbit-community": {
            metadata: {
              preview: false,
              vip: "",
              title: "Urbit Community",
              description: "World hub, help desk, meet and greet, etc.",
              creator: "~bitbet-bolbel",
              picture:
                "https://fabled-faster.nyc3.digitaloceanspaces.com/fabled-faster/2021.4.02..21.52.41-UC.png",
              hidden: false,
              config: {
                group: {
                  "app-name": "graph",
                  resource: "/ship/~bitbet-bolbel/urbit-community-5.963",
                },
              },
              "date-created": "~2020.6.25..21.39.35..2fd2",
              color: "0x8f.9c9d",
            },
            "app-name": "groups",
            resource: "/ship/~bitbet-bolbel/urbit-community",
            group: "/ship/~bitbet-bolbel/urbit-community",
          },
        },
        graph: {
          "/ship/~bitbet-bolbel/links": {
            metadata: {
              preview: false,
              vip: "",
              title: "Link Collection",
              description: "",
              creator: "~darrux-landes",
              picture: "",
              hidden: false,
              config: {
                graph: "link",
              },
              "date-created": "~2020.4.6..21.53.30..dc68",
              color: "0x0",
            },
            "app-name": "graph",
            resource: "/ship/~bitbet-bolbel/links",
            group: "/ship/~bitbet-bolbel/urbit-community",
          },
          "/ship/~darrux-landes/development": {
            metadata: {
              preview: false,
              vip: "",
              title: "Development",
              description:
                "Urbit Development Mailing List: https://groups.google.com/a/urbit.org/forum/#!forum/dev",
              creator: "~darrux-landes",
              picture: "",
              hidden: false,
              config: {
                graph: "chat",
              },
              "date-created": "~2020.4.6..21.53.30..dc68",
              color: "0x0",
            },
            "app-name": "graph",
            resource: "/ship/~darrux-landes/development",
            group: "/ship/~bitbet-bolbel/urbit-community",
          },
        },
      },
      previews: {
        "/ship/~bollug-worlus/urbit-index": groupPreview,
        "/ship/~bollug-worlus/urbit-index-start": groupPreview,
        "/ship/~bollug-worlus/urbit-index-metadata": groupPreview,
        "/ship/~bollug-worlus/urbit-index-done": groupPreview,
        "/ship/~bollug-worlus/urbit-index-error": groupPreview,
      },
    });

    useContactState.setState({
      contacts: {
        "~sampel-palnet": {
          status: "Just urbiting",
          "last-updated": 1621511447583,
          avatar: null,
          cover: null,
          bio: "An urbit user",
          nickname: "Sample Planet",
          color: "0xee.5432",
          groups: [],
        },
      },
    });

    useGraphState.setState({
      looseNodes: {
        "darrux-landes/development": {
          "/170141184505059416342852185329797955584": {
            post: {
              index: "/170141184505059416342852185329797955584",
              author: "sipfyn-pidmex",
              "time-sent": 1621275183241,
              signatures: [
                {
                  signature:
                    "0x3.9e41.4f04.3cac.786e.30c1.f4cc.8db3.9a78.0401.d16f.6301.94d0.a08a.0695.5008.02bf.0e07.a7a9.3d87.85f7.6334.e598.4ed3.5dee.58a7.cbd3.30e6.d65b.1fc9.ac62.162a.daf0.ff14.9cca.4a93.8177.0755.7b74.9d52.c0a6.b27f.9001",
                  life: 2,
                  ship: "sipfyn-pidmex",
                },
              ],
              contents: [
                {
                  text:
                    "is there a way to get a bunt of a specific instantance of a tagged union? i.e. if you have `$%([%a =atom] [%b =cell])`, can you get a bunt of specifically subtype `%a`?",
                },
              ],
              hash: "0xe790.53c1.0f2b.1e1b.8c30.7d33.236c.e69e",
            },
            children: {
              root: {},
              cachedIter: null,
            },
          },
        },
      },
    });

    return (
      <ThemeProvider theme={theme}>
        <BrowserRouter>
          <Story />
          <div id="portal-root" />
        </BrowserRouter>
      </ThemeProvider>
    );
  },
];
