import React, { Component, PureComponent } from "react";
import moment from "moment";
import _ from "lodash";
import { Box, Row, Text, Rule } from "@tlon/indigo-react";

import OverlaySigil from '~/views/components/OverlaySigil';
import { uxToHex, cite, writeText, useShowNickname } from '~/logic/lib/util';
import { Group, Association, Contacts, Post } from "~/types";
import TextContent from './content/text';
import CodeContent from './content/code';
import RemoteContent from '~/views/components/RemoteContent';
import { Mention } from "~/views/components/MentionText";
import styled from "styled-components";
import useLocalState from "~/logic/state/local";

export const DATESTAMP_FORMAT = '[~]YYYY.M.D';

export const UnreadMarker = React.forwardRef((props, ref) => (
  <Row flexShrink={0} ref={ref} color='blue' alignItems='center' fontSize={0} position='absolute' width='100%' py={2}>
    <Rule borderColor='blue' display={['none', 'block']} m={0} width='2rem' />
    <Text flexShrink={0} display='block' zIndex={2} mx={4} color='blue'>New messages below</Text>
    <Rule borderColor='blue' flexGrow={1} m={0}/>
    <Rule style={{ width: "calc(50% - 48px)" }} borderColor='blue' m={0} />
  </Row>
));

export const DayBreak = ({ when }) => (
  <Row pb='3' alignItems="center" justifyContent="center" width='100%'>
    <Text gray>{moment(when).calendar(null, { sameElse: DATESTAMP_FORMAT })}</Text>
  </Row>
);

interface ChatMessageProps {
  measure(element): void;
  msg: Post;
  previousMsg?: Post;
  nextMsg?: Post;
  isLastRead: boolean;
  group: Group;
  association: Association;
  contacts: Contacts;
  className?: string;
  isPending: boolean;
  style?: any;
  scrollWindow: HTMLDivElement;
  isLastMessage?: boolean;
  unreadMarkerRef: React.RefObject<HTMLDivElement>;
  history: any;
  api: any;
  highlighted?: boolean;
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
      className = '',
      isPending,
      style,
      measure,
      scrollWindow,
      isLastMessage,
      unreadMarkerRef,
      history,
      api,
      highlighted,
      fontSize
    } = this.props;

    const renderSigil = Boolean((nextMsg && msg.author !== nextMsg.author) || !nextMsg || msg.number === 1);
    const dayBreak = nextMsg && new Date(msg['time-sent']).getDate() !== new Date(nextMsg['time-sent']).getDate();

    const containerClass = `${renderSigil
      ? `cf pl2 lh-copy`
      : `items-top cf hide-child`} ${isPending ? 'o-40' : ''} ${className}`

    const timestamp = moment.unix(msg['time-sent'] / 1000).format(renderSigil ? 'hh:mm a' : 'hh:mm');

    const reboundMeasure = (event) => {
      return measure(this.divRef.current);
    };

    const messageProps = {
      msg,
      timestamp,
      contacts,
      association,
      group,
      measure: reboundMeasure.bind(this),
      style,
      containerClass,
      isPending,
      history,
      api,
      scrollWindow,
      highlighted,
      fontSize
    };

    const unreadContainerStyle = {
      height: isLastRead ? '2rem' : '0',
    };

    return (
      <Box
        bg={highlighted ? 'washedBlue' : 'white'}
        flexShrink={0}
        width='100%'
        display='flex'
        flexWrap='wrap'
        pt={this.props.pt ? this.props.pt : renderSigil ? 3 : 0}
        pr={3}
        pb={isLastMessage ? 3 : 0}
        ref={this.divRef}
        className={containerClass}
        style={style}
        mb={1}
      >
        {dayBreak && !isLastRead ? <DayBreak when={msg['time-sent']} /> : null}
        {renderSigil
          ? <MessageWithSigil {...messageProps} />
          : <MessageWithoutSigil {...messageProps} />}
        <Box flexShrink={0} fontSize={0} position='relative' width='100%' overflow='visible' style={unreadContainerStyle}>{isLastRead
          ? <UnreadMarker dayBreak={dayBreak} when={msg['time-sent']} ref={unreadMarkerRef} />
          : null}</Box>
      </Box>
    );
  }
}

interface MessageProps {
  msg: Post;
  timestamp: string;
  group: Group;
  association: Association;
  contacts: Contacts;
  containerClass: string;
  isPending: boolean;
  style: any;
  measure(element): void;
  scrollWindow: HTMLDivElement;
};

export const MessageWithSigil = (props) => {
  const {
    msg,
    timestamp,
    contacts,
    association,
    group,
    measure,
    api,
    history,
    scrollWindow,
    fontSize
  } = props;

  const dark = useLocalState(state => state.dark);

  const datestamp = moment.unix(msg['time-sent'] / 1000).format(DATESTAMP_FORMAT);
  const contact = msg.author in contacts ? contacts[msg.author] : false;
  const showNickname = useShowNickname(contact);
  const name = showNickname ? contact.nickname : cite(msg.author);
  const color = contact ? `#${uxToHex(contact.color)}` : dark ?  '#000000' :'#FFFFFF'
  const sigilClass = contact ? '' : dark ? 'mix-blend-diff' : 'mix-blend-darken';

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
        group={group}
        scrollWindow={scrollWindow}
        history={history}
        api={api}
        bg="white"
        className="fl pr3 v-top pt1"
      />
      <Box flexGrow={1} display='block' className="clamp-message">
        <Box
          flexShrink={0}
          className="hide-child"
          pt={1}
          pb={1}
          display='flex'
          alignItems='center'
        >
          <Text
            fontSize={0}
            mr={3}
            flexShrink={0}
            mono={!showNickname}
            fontWeight={(showNickname) ? '500' : '400'}
            className={`mw5 db truncate pointer`}
            ref={e => nameSpan = e}
            onClick={() => {
              writeText(`~${msg.author}`);
              copyNotice(name);
            }}
            title={`~${msg.author}`}
          >{name}</Text>
          <Text flexShrink='0' gray mono className="v-mid">{timestamp}</Text>
          <Text flexShrink={0}  gray mono ml={2} className="v-mid child dn-s">{datestamp}</Text>
        </Box>
        <ContentBox flexShrink={0} fontSize={fontSize ? fontSize : '14px'}>
          {msg.contents.map(c =>
          <MessageContent
            contacts={contacts}
            content={c}
            measure={measure}
            fontSize={fontSize}
            group={group}
          />)}
        </ContentBox>
      </Box>
    </>
  );
}


const ContentBox = styled(Box)`
  & > :first-child {
    margin-left: 0px;
  }

`;

export const MessageWithoutSigil = ({ timestamp, contacts, msg, measure, group }) => (
  <>
    <Text flexShrink={0} mono gray display='inline-block' pt='2px' lineHeight='tall' className="child">{timestamp}</Text>
    <ContentBox flexShrink={0} fontSize='14px' className="clamp-message" style={{ flexGrow: 1 }}>
      {msg.contents.map((c, i) => (
        <MessageContent
          key={i}
          contacts={contacts}
          content={c}
          group={group}
          measure={measure}/>))}
    </ContentBox>
  </>
);

export const MessageContent = ({ content, contacts, measure, fontSize, group }) => {
  if ('code' in content) {
    return <CodeContent content={content} />;
  } else if ('url' in content) {
    return (
      <Box mx="2px" flexShrink={0} fontSize={fontSize ? fontSize : '14px'} lineHeight="tall" color='black'>
        <RemoteContent
          url={content.url}
          onLoad={measure}
          imageProps={{style: {
            maxWidth: '18rem',
            display: 'block'
          }}}
          videoProps={{style: {
            maxWidth: '18rem',
            display: 'block'
          }
          }}
          textProps={{style: {
            fontSize: 'inherit',
            textDecoration: 'underline'
          }}}
        />
      </Box>
    );
  } else if ('text' in content) {
    return <TextContent fontSize={fontSize} content={content} />;
  } else if ('mention' in content) {
    return <Mention group={group} ship={content.mention} contact={contacts?.[content.mention]} />
  } else {
    return null;
  }
};

export const MessagePlaceholder = ({ height, index, className = '', style = {}, ...props }) => (
  <Box
    width='100%'
    fontSize='2'
    pl='3' pt='4'
    pr='3'
    display='flex'
    lineHeight='tall'
    className={className}
    style={{ height, ...style }}
    {...props}
  >
      <Box pr='3' verticalAlign='top' backgroundColor='white' style={{ float: 'left' }}>
        <Text
          display='block'
          background='gray'
          width='24px'
          height='24px'
          borderRadius='50%'
          style={{
            visibility: (index % 5 == 0) ? "initial" : "hidden",
          }}
        ></Text>
      </Box>
      <Box
        style={{ float: 'right', flexGrow: 1 }}
        color='black'
        className="clamp-message"
      >
        <Box
          className="hide-child"
          paddingTop='4'
          style={{visibility: (index % 5 == 0) ? "initial" : "hidden" }}
        >
          <Text
            display='inline-block'
            verticalAlign='middle'
            fontSize='0'
            gray
            cursor='default'
          >
            <Text maxWidth='32rem' display='block'>
              <Text
                backgroundColor='gray'
                display='block'
                width='100%'
                height='100%'></Text>
              </Text>
          </Text>
          <Text
            display='inline-block'
            mono
            verticalAlign='middle'
            fontSize='0'
            gray
          >
            <Text
              background='gray'
              display='block'
              height='1em'
              style={{ width: `${(index % 3 + 1) * 3}em` }}
            ></Text>
            </Text>
          <Text
            mono
            verticalAlign='middle'
            fontSize='0'
            ml='2'
            gray
            display={['none', 'inline-block']}
            className="child">
            <Text
              backgroundColor='gray'
              display='block'
              width='100%'
              height='100%'
              ></Text>
            </Text>
        </Box>
        <Text
          display='block'
          backgroundColor='gray'
          height='1em'
          style={{ width: `${(index % 5) * 20}%` }}></Text>
      </Box>
    </Box>
);
