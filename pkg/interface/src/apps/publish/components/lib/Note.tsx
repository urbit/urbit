import React, { Component, useState, useEffect } from "react";
import { Box, Text, Col } from "@tlon/indigo-react";
import ReactMarkdown from 'react-markdown';
import { Link, RouteComponentProps } from "react-router-dom";
import { SidebarSwitcher } from "../../../../components/SidebarSwitch";
import { Spinner } from "../../../../components/Spinner";
import { Comments } from "./Comments";
import { NoteNavigation } from "./NoteNavigation";
import moment from "moment";
import { cite } from "../../../../lib/util";
import { PublishContent } from "./PublishContent";
import { NoteId, Note as INote, Notebook } from "../../../../types/publish-update";
import { Contacts } from "../../../../types/contact-update";
import GlobalApi from "../../../../api/global";

interface NoteProps {
  ship: string;
  book: string;
  noteId: NoteId;
  note: INote;
  notebook: Notebook;
  contacts: Contacts;
  api: GlobalApi;
}

export function Note(props: NoteProps & RouteComponentProps) {
  const [deleting, setDeleting] = useState(false);

  const { notebook, note, contacts, ship, book, noteId, api } = props;
  // fetch note and read actions
  //
  useEffect(() => {
    api.publish.fetchNote(ship, book, noteId);
  }, [ship, book, noteId]);

  const baseUrl = `/~publish/notebook/${props.ship}/${props.book}`;

  const deletePost = async () => {
    setDeleting(true);
    const deleteAction = {
      "del-note": {
        who: props.ship.slice(1),
        book: props.book,
        note: props.noteId,
      },
    };
    await api.publish.publishAction(deleteAction);
    props.history.push(baseUrl);
  };

  const comments = note?.comments || false;
  const title = note?.title || "";
  const author = note?.author || "";
  const file = note?.file || "";
  const date = moment(note?.["date-created"]).fromNow() || 0;

  const contact =
    author.substr(1) in props.contacts
      ? props.contacts[author.substr(1)]
      : null;

  let name = author;
  if (contact) {
    name = contact.nickname.length > 0 ? contact.nickname : author;
  }

  if (name === author) {
    name = cite(author);
  }

  if (!file) {
    return null;
  }

  const newfile = file.slice(file.indexOf(";>") + 2);
  const prevId = note?.["prev-note"] || null;
  const nextId = note?.["next-note"] || null;
  const prevDate =
    prevId ? moment(notebook?.notes?.[prevId]?.["date-created"]).fromNow() : 0;
  const nextDate =
    nextId ? moment(notebook?.notes?.[nextId]?.["date-created"]).fromNow() : 0;

  const prev =
    prevId === null
      ? null
      : {
          id: prevId,
          title: notebook?.notes?.[prevId]?.title,
          date: prevDate,
        };
  const next =
    nextId === null
      ? null
      : {
          id: nextId,
          title: notebook?.notes?.[nextId]?.title,
          date: nextDate,
        };

  let editPost = null;
  const editUrl = props.location.pathname + "/edit";
  if (`~${window.ship}` === author) {
    editPost = (
      <div className="dib">
        <Link className="green2 f9" to={editUrl}>
          Edit
        </Link>
        <p
          className="dib f9 red2 ml2 pointer"
          onClick={() => deletePost()}
        >
          Delete
        </p>
      </div>
    );
  }

  return (
    <Box
      my={3}
      display="grid"
      gridTemplateColumns="1fr"
      gridAutoRows="auto"
      maxWidth="500px"
      width="100%"
      gridRowGap={4}
    >
      <Link to={baseUrl}>
        <Text>{"<- Notebook Index"}</Text>
      </Link>
      <Col>
        <Box mb={2}>{title}</Box>
        <Box display="flex">
          <Text
            color="gray"
            mr={2}
            fontFamily={contact?.nickname ? "sans" : "mono"}
          >
            {name}
          </Text>
          <Text className="di" style={{ lineHeight: 1 }}>
            <Text color="gray">{date}</Text>
            <Text ml={2}>{editPost}</Text>
          </Text>
        </Box>
      </Col>
      <Box className="md" style={{ overflowWrap: "break-word" }}>
        <ReactMarkdown source={newfile} linkTarget={"_blank"} />
      </Box>
      <NoteNavigation
        prev={prev}
        next={next}
        ship={props.ship}
        book={props.book}
      />
      <Comments
        enabled={notebook.comments}
        ship={props.ship}
        book={props.book}
        noteId={props.noteId}
        note={props.note}
        comments={comments}
        numComments={props.note?.["num-comments"]}
        contacts={props.contacts}
        api={props.api}
      />
      <Spinner
        text="Deleting post..."
        awaiting={deleting}
        classes="absolute bottom-1 right-1 ba b--gray1-d pa2"
      />
    </Box>
  );
}

export default Note;
