import React, { useCallback, useState, useRef, useEffect } from "react";
import { Col, Text, ErrorMessage } from "@tlon/indigo-react";
import { Spinner } from "~/views/components/Spinner";
import { Notebooks } from "~/types/publish-update";
import { useWaitForProps } from "~/logic/lib/useWaitForProps";
import { RouteComponentProps } from "react-router-dom";

interface JoinScreenProps {
  api: any; // GlobalApi;
  ship: string;
  book: string;
  notebooks: Notebooks;
}

export function JoinScreen(props: JoinScreenProps & RouteComponentProps) {
  const { book, ship, api } = props;
  const [error, setError] = useState(false);
  const joining = useRef(false);

  const waiter = useWaitForProps(props);

  const onJoin = useCallback(async () => {
    joining.current = true;
    const action = {
      subscribe: {
        who: ship.replace("~", ""),
        book,
      },
    };

    try {
      await api.publish.publishAction(action);
      await waiter((p) => !!p.notebooks?.[ship]?.[book]);
      props.history.push(`/~publish/notebook/${ship}/${book}`);
    } catch (e) {
      console.error(e);
      setError(true);
    } finally {
      joining.current = false;
    }
  }, [waiter, api, ship, book]);

  useEffect(() => {
    if (joining.current) {
      return;
    }
    onJoin();
  }, [onJoin]);

  return (
    <Col p={4}>
      <Text fontSize={1}>Joining Notebook</Text>
      <Spinner awaiting text="Joining..." />
      {error && <ErrorMessage>Unable to join notebook</ErrorMessage>}
    </Col>
  );
}

export default JoinScreen;
