Getting Started with Eta
======================

In this tutorial, you'll compile a simple program with Eta and run it, illustrating the basic workflows.

In Eta, there are primarily two workflows:

#. Trying out simple programs without any dependencies. (EPM not needed)

#. Working on a multi-file, multi-dependency project. (EPM needed)

.. code::

      main :: IO ()
      main = print $ quicksort [1, 123, 42, 90, 24, 12, 3]

      quicksort [] = []
      quicksort (x:xs) = quicksort left ++ [x] ++ quicksort right
        where left  = [ y | y <- xs, y < x ]
              right = [ y | y <- xs, y > x ]

      newtype Feet = Feet Double
      newtype Meters = Meters Double

      -- Conversions between measurements.
      feet2Meters :: Feet -> Meters
      feet2Meters (Feet ft) = Meters $ ft * 0.3048

      meters2Feet :: Feet -> Meters
      meters2Feet (Meters m) = Feet $ m * 0.3048

      -- Feet and Meters have the exact same representation
      -- but the compiler will not allow Feet to passed
      -- into this function.
      volumeOfShippingBox :: Meters -> Meters
      volumeOfShippingBox (Meters sideLength) =
        Meters $ sideLength * sideLength * sideLength

      main :: IO ()
      main = do
        -- Extract the first command-line argument.
        ("-n":arg:_) <- getArgs

        -- Convert it to an integer.
        let n = read arg :: Int

        -- Read the contents of standard input lazily.
        oldContents <- getContents

        -- Process the first `n` lines in a pipeline.
        let newContents = unlines . take n . lines $ contents

        -- Print the result.
        putStr newContents

      import Control.Concurrent.STM

      type Account = TVar USD
      type USD = Double

      transferMoney :: USD -> Account -> Account -> IO ()
      transferMoney amount sender reciever =
        atomically $ transact amount sender receiver

      transact :: Dollars -> Account -> Account -> STM ()
      transact amount sender receiever = do
        senderBalance <- readTVar sender
        receiverBalance <- readTVar sender
        writeTVar sender (senderBalance - amount)
        writeTVar receiver (receiverBalance + amount)


      type instance IxValue (e -> a) = a
      instance Eq e => Ixed (e -> a) where
        ix e p f = p (f e) <&> \a e' -> if e == e' then a else f e'
        {-# INLINE ix #-}

      type instance IxValue (Maybe a) = a
      instance Ixed (Maybe a) where
        ix () f (Just a) = Just <$> f a
        ix () _ Nothing  = pure Nothing
        {-# INLINE ix #-}

      type instance IxValue [a] = a
      instance Ixed [a] where
        ix k f xs0 | k < 0     = pure xs0
                  | otherwise = go xs0 k where
          go [] _ = pure []
          go (a:as) 0 = f a <&> (:as)
          go (a:as) i = (a:) <$> (go as $! i - 1)
        {-# INLINE ix #-}

      type instance IxValue (NonEmpty a) = a
      instance Ixed (NonEmpty a) where
        ix k f xs0 | k < 0 = pure xs0
                  | otherwise = go xs0 k where
          go (a:|as) 0 = f a <&> (:|as)
          go (a:|as) i = (a:|) <$> ix (i - 1) f as
        {-# INLINE ix #-}

      type instance IxValue (Identity a) = a
      instance Ixed (Identity a) where
        ix () f (Identity a) = Identity <$> f a
        {-# INLINE ix #-}

      type instance IxValue (Tree a) = a
      instance Ixed (Tree a) where
        ix xs0 f = go xs0 where
          go [] (Node a as) = f a <&> \a' -> Node a' as
          go (i:is) t@(Node a as)
            | i < 0     = pure t
            | otherwise = Node a <$> ix i (go is) as
        {-# INLINE ix #-}

      -- | A 'Simple' 'Prism'.
      type Prism' s a = Prism s s a a

      -------------------------------------------------------------------------------
      -- Equality
      -------------------------------------------------------------------------------

      -- | A witness that @(a ~ s, b ~ t)@.
      --
      -- Note: Composition with an 'Equality' is index-preserving.
      #if __GLASGOW_HASKELL__ >= 800
      type Equality (s :: k1) (t :: k2) (a :: k1) (b :: k2) = forall k3 (p :: k1 -> k3 -> *) (f :: k2 -> k3) .
      #elif __GLASGOW_HASKELL__ >= 706
      type Equality (s :: k1) (t :: k2) (a :: k1) (b :: k2) = forall (p :: k1 -> * -> *) (f :: k2 -> *) .
      #else
      type Equality s t a b = forall p (f :: * -> *) .
      #endif
          p a (f b) -> p s (f t)

      -- | A 'Simple' 'Equality'.
      type Equality' s a = Equality s s a a

      -- | Composable `asTypeOf`. Useful for constraining excess
      -- polymorphism, @foo . (id :: As Int) . bar@.
      type As a = Equality' a a


      {-# LANGUAGE TypeSynonymInstances #-}
      module Network.UDP
      ( DataPacket(..)
      , openBoundUDPPort
      , openListeningUDPPort
      , pingUDPPort
      , sendUDPPacketTo
      , recvUDPPacket
      , recvUDPPacketFrom
      ) where

      import qualified Data.ByteString as Strict (ByteString, concat, singleton)
      import qualified Data.ByteString.Lazy as Lazy (ByteString, toChunks, fromChunks)
      import Data.ByteString.Char8 (pack, unpack)
      import Network.Socket hiding (sendTo, recv, recvFrom)
      import Network.Socket.ByteString (sendTo, recv, recvFrom)

      -- Type class for converting StringLike types to and from strict ByteStrings
      class DataPacket a where
        toStrictBS :: a -> Strict.ByteString
        fromStrictBS :: Strict.ByteString -> a

      instance DataPacket Strict.ByteString where
        toStrictBS = id
        {-# INLINE toStrictBS #-}
        fromStrictBS = id
        {-# INLINE fromStrictBS #-}

      openBoundUDPPort :: String -> Int -> IO Socket
      openBoundUDPPort uri port = do
        s <- getUDPSocket
        bindAddr <- inet_addr uri
        let a = SockAddrInet (toEnum port) bindAddr
        bindSocket s a
        return s

      pingUDPPort :: Socket -> SockAddr -> IO ()
      pingUDPPort s a = sendTo s (Strict.singleton 0) a >> return ()

Without EPM
------------

#. Create a new file called *Main.hs* and put the following as its contents::

    module Main where

    primes = filterPrime [2..]
      where filterPrime (p:xs) =
              p : filterPrime [x | x <- xs, x `mod` p /= 0]

    main = putStrLn $ "The 101st prime is " ++ show (primes !! 100)

#. Run the following command on the command line to compile the program::

    eta -o Out.jar Main.hs

   This will compile the program to a standalone JAR.

#. Run the program with java::

    java -classpath Out.jar eta.main

   The ``eta.main`` class contains the entry point that initializes the Eta runtime
   system and runs the compiled program.

With EPM
---------

With EPM, things become *much* smoother.

#. Create a new directory called ``eta-first`` and enter it.

   .. code::

      mkdir eta-first
      cd eta-first

#. Initialize the project with EPM.

   .. code:: bash

      epm init

   This is an interactive command that will ask you questions and help you generate
   a cabal project file for your project. Select **Executable** (option 2) for project
   type and ``src`` for the source directory. The project structure should look
   like this::

      eta-first/
      |--src/
      |--eta-first.cabal
      |

   Your directory structure may vary based on the options you chose, such as the
   license type.

#. Add the files ``Main.hs`` and ``Primes.hs`` in ``src/`` as shown below.

   ``Main.hs``
   .. code::

    module Main where

    import Primes

    main = putStrLn $ "The 101st prime is " ++ show (primes !! 100)

   ``Primes.hs``
   .. code::

      module Primes where

      primes = filterPrime [2..]
        where filterPrime (p:xs) =
                p : filterPrime [x | x <- xs, x `mod` p /= 0]

#. Update ``eta-first.cabal``, adding an ``other-modules:`` field::

      other-modules: Primes```

#. To build & run, execute this command::

    epm run

#. That build may have been slow. In order to make the build faster, configure the
   project to make a dynamic executable::

    epm configure --enable-executable-dynamic
    epm run

   Note that you don't have to run ``configure`` again from then on unless you want to
   revert it back to uberjar-mode with ``--disable-executable-dynamic``.

Contact Us
==========

If you had trouble with this tutorial, you can give us feedback by:

- filing an `issue <https://github.com/typelead/eta/issues/new>`_
- discussing with us on `Gitter <https://gitter.im/typelead/eta>`_
