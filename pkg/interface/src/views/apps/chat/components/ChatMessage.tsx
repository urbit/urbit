import React, { Component, PureComponent } from "react";
import moment from "moment";
import _ from "lodash";
import { Box, Row, Text } from "@tlon/indigo-react";

import { OverlaySigil } from './overlay-sigil';
import { uxToHex, cite, writeText } from '~/logic/lib/util';
import { Envelope, IMessage } from "~/types/chat-update";
import { Group, Association, Contacts, LocalUpdateRemoteContentPolicy } from "~/types";
import TextContent from './content/text';
import CodeContent from './content/code';
import RemoteContent from '~/views/components/RemoteContent';

export const DATESTAMP_FORMAT = '[~]YYYY.M.D';

export const UnreadMarker = React.forwardRef(({ dayBreak, when }, ref) => (
  <div ref={ref} style={{ color: "#219dff" }} className="flex items-center f9 absolute w-100 left-0 pv0">
    <hr style={{ borderColor: "#219dff" }} className="dn-s ma0 w2 bt-0" />
    <p className="mh4 z-2" style={{ whiteSpace: 'normal' }}>New messages below</p>
    <hr style={{ borderColor: "#219dff" }} className="ma0 flex-grow-1 bt-0" />
    {dayBreak
      ? <p className="gray2 mh4">{moment(when).calendar()}</p>
      : null}
    <hr style={{ width: "calc(50% - 48px)" }} style={{ borderColor: "#219dff" }} className="ma0 bt-0" />
  </div>
));

export const DayBreak = ({ when }) => (
  <div className="pv3 gray2 b--gray2 flex items-center justify-center f9 w-100">
    <p>{moment(when).calendar()}</p>
  </div>
);

interface ChatMessageProps {
  measure(element): void;
  msg: Envelope | IMessage;
  previousMsg: Envelope | IMessage | undefined;
  nextMsg: Envelope | IMessage | undefined;
  isLastRead: boolean;
  group: Group;
  association: Association;
  contacts: Contacts;
  hideAvatars: boolean;
  hideNicknames: boolean;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
  className?: string;
  isPending: boolean;
  style?: any;
  allStations: any;
  scrollWindow: HTMLDivElement;
  isLastMessage?: boolean;
  unreadMarkerRef: React.RefObject<HTMLDivElement>;
  history: any;
  api: any;
}

export default class ChatMessage extends Component<ChatMessageProps> {
  private divRef: React.RefObject<HTMLDivElement>;

  constructor(props) {
    super(props);
    this.divRef = React.createRef();
  }

  componentDidMount() {
    if (this.divRef.current) {
      this.props.measure(this.divRef.current);
    }
  }

  render() {
    const {
      msg,
      previousMsg,
      nextMsg,
      isLastRead,
      group,
      association,
      contacts,
      hideAvatars,
      hideNicknames,
      remoteContentPolicy,
      className = '',
      isPending,
      style,
      measure,
      scrollWindow,
      isLastMessage,
      unreadMarkerRef,
      allStations,
      history,
      api
    } = this.props;

    const renderSigil = Boolean((nextMsg && msg.author !== nextMsg.author) || !nextMsg || msg.number === 1);
    const dayBreak = nextMsg && new Date(msg.when).getDate() !== new Date(nextMsg.when).getDate();

    const containerClass = `${renderSigil
      ? `cf pt2 pl3 lh-copy`
      : `items-center cf hide-child`} ${isPending ? 'o-40' : ''} ${className}`

    const timestamp = moment.unix(msg.when / 1000).format(renderSigil ? 'hh:mm a' : 'hh:mm');

    const reboundMeasure = (event) => {
      return measure(this.divRef.current);
    };

    const messageProps = {
      msg,
      timestamp,
      contacts,
      hideNicknames,
      association,
      group,
      hideAvatars,
      remoteContentPolicy,
      measure: reboundMeasure.bind(this),
      style,
      containerClass,
      isPending,
      allStations,
      history,
      api,
      scrollWindow
    };

    const unreadContainerStyle = {
      height: isLastRead ? '1.66em' : '0',
    };

    return (
      <Box
        width='100%'
        display='flex'
        flexWrap='wrap'
        pr={3}
        pb={isLastMessage ? 3 : 0}
        ref={this.divRef}
        className={containerClass}
        style={style}
        data-number={msg.number}
        mb={1}
      >
        {dayBreak && !isLastRead ? <DayBreak when={msg.when} /> : null}
        {renderSigil
          ? <MessageWithSigil {...messageProps} />
          : <MessageWithoutSigil {...messageProps} />}
        <Box fontSize={0} position='relative' width='100%' overflow='visible' style={unreadContainerStyle}>{isLastRead
          ? <UnreadMarker dayBreak={dayBreak} when={msg.when} ref={unreadMarkerRef} />
          : null}</Box>
      </Box>
    );
  }
}

interface MessageProps {
  msg: Envelope | IMessage;
  timestamp: string;
  group: Group;
  association: Association;
  contacts: Contacts;
  hideAvatars: boolean;
  hideNicknames: boolean;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
  containerClass: string;
  isPending: boolean;
  style: any;
  allStations: any;
  measure(element): void;
  scrollWindow: HTMLDivElement;
};

export class MessageWithSigil extends PureComponent<MessageProps> {
  render() {
    const {
      msg,
      timestamp,
      contacts,
      hideNicknames,
      association,
      group,
      hideAvatars,
      remoteContentPolicy,
      measure,
      allStations,
      history,
      api,
      scrollWindow
    } = this.props;

    const datestamp = moment.unix(msg.when / 1000).format(DATESTAMP_FORMAT);
    const contact = msg.author in contacts ? contacts[msg.author] : false;
    const showNickname = !hideNicknames && contact && contact.nickname;
    const name = showNickname ? contact.nickname : cite(msg.author);
    const color = contact ? `#${uxToHex(contact.color)}` : '#000000';
    const sigilClass = contact ? '' : 'mix-blend-diff';

    let nameSpan = null;

    const copyNotice = (saveName) => {
      if (nameSpan !== null) {
        nameSpan.innerText = 'Copied';
        setTimeout(() => {
          nameSpan.innerText = saveName;
        }, 800);
      }
    };

    return (
      <>
        <OverlaySigil
          ship={msg.author}
          contact={contact}
          color={color}
          sigilClass={sigilClass}
          association={association}
          group={group}
          hideAvatars={hideAvatars}
          hideNicknames={hideNicknames}
          scrollWindow={scrollWindow}
          allStations={allStations}
          history={history}
          api={api}
          className="fl pr3 v-top bg-white bg-gray0-d pt1"
        />
        <div className="clamp-message" style={{ flexGrow: 1 }}>
          <Box
            className="hide-child"
            pt={1}
            pb={1}
            display='flex'
            alignItems='center'
          >
            <Text
              fontSize={0}
              mr={3}
              mono={!showNickname}
              className={`mw5 db truncate pointer`}
              ref={e => nameSpan = e}
              onClick={() => {
                writeText(msg.author);
                copyNotice(name);
              }}
              title={`~${msg.author}`}
            >{name}</Text>
            <Text gray mono className="v-mid">{timestamp}</Text>
            <Text gray mono ml={2} className="v-mid child dn-s">{datestamp}</Text>
          </Box>
          <Box fontSize={0}><MessageContent content={msg.letter} remoteContentPolicy={remoteContentPolicy} measure={measure} /></Box>
        </div>
      </>
    );
  }
}

export const MessageWithoutSigil = ({ timestamp, msg, remoteContentPolicy, measure }) => (
  <>
    <p className="child pr1 mono f9 gray2 dib">{timestamp}</p>
    <Box fontSize={0} className="clamp-message" style={{ flexGrow: 1 }}>
      <MessageContent content={msg.letter} remoteContentPolicy={remoteContentPolicy} measure={measure}/>
    </Box>
  </>
);

export const MessageContent = ({ content, remoteContentPolicy, measure }) => {
  if ('code' in content) {
    return <CodeContent content={content} />;
  } else if ('url' in content) {
    return (
      <Text fontSize={0} lineHeight="tall" color='gray'>
        <RemoteContent
          url={content.url}
          remoteContentPolicy={remoteContentPolicy}
          onLoad={measure}
          imageProps={{style: {
              maxWidth: '18rem'
          }}}
          videoProps={{style: {
            maxWidth: '18rem'
          }}}
        />
      </Text>
    );
  } else if ('me' in content) {
    return (
      <p className='f9 i lh-copy v-top'>
        {content.me}
      </p>
    );
  }
  else if ('text' in content) {
    return <TextContent content={content} />;
  } else {
    return null;
  }
};

export const MessagePlaceholder = ({ height, index, className = '', style = {}, ...props }) => (
  <div className={`w-100 f7 pl3 pt4 pr3 cf flex lh-copy ${className}`} style={{ height, ...style }} {...props}>
      <div className="fl pr3 v-top bg-white bg-gray0-d">
        <span
          className="db bg-gray2 bg-white-d"
          style={{
            width: "24px",
            height: "24px",
            borderRadius: "50%",
            visibility: (index % 5 == 0) ? "initial" : "hidden",
          }}
        ></span>
      </div>
      <div className="fr clamp-message white-d" style={{ flexGrow: 1, marginTop: -8 }}>
        <div className="hide-child" style={{paddingTop: "6px", visibility: (index % 5 == 0) ? "initial" : "hidden" }}>
          <p className={`v-mid f9 gray2 dib mr3 c-default`}>
            <span className="mw5 db"><span className="bg-gray5 bg-gray1-d db w-100 h-100"></span></span>
          </p>
          <p className="v-mid mono f9 gray2 dib"><span className="bg-gray5 bg-gray1-d db w-100 h-100" style={{height: "1em", width: `${(index % 3 + 1) * 3}em`}}></span></p>
          <p className="v-mid mono f9 ml2 gray2 dib child dn-s"><span className="bg-gray5 bg-gray1-d db w-100 h-100"></span></p>
        </div>
        <span className="bg-gray5 bg-gray1-d db w-100 h-100 db" style={{height: `1em`, width: `${(index % 5) * 20}%`}}></span>
      </div>
    </div>
);
