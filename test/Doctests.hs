import Test.DocTest (doctest)

docTestQueueModel :: IO ()
docTestQueueModel = doctest ["tutorials/QueueModel.lhs"]

docTestOptionPricing :: IO ()
docTestOptionPricing = doctest ["tutorials/OptionPricing.lhs"]

docTestGeneTranscription :: IO ()
docTestGeneTranscription = doctest ["tutorials/GeneTranscription.lhs"]
